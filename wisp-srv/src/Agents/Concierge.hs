module Agents.Concierge
  ( -- Deterministic flows
    classifyAllPending
  , classifyPending
    -- Decision flows
  , handleChat
    -- Types
  , ConciergeToolCall(..)
  , ActivityUpdates(..)
  , ActivityFilter(..)
  , PeopleFilter(..)
  , ToolResult(..)
    -- Dispatcher
  , executeToolCall
  ) where

import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), decode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import App.Monad (App, Env(..))
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..), ActivityStatus)
import qualified Domain.Activity as Activity
import Domain.Classification (Classification(..))
import Domain.Id (EntityId(..), unEntityId)
import Domain.Person (Person(..))
import Domain.Receipt (NewReceipt(..), ReceiptAction(..))
import Infra.Db.Activity (getActivitiesByStatus, getActivitiesFiltered, updateActivityClassification, updateActivityStatus, getActivity, insertConversation, getTodaysCalendarEvents, getRecentActivities)
import Infra.Db.Person (searchPeople, getPersonByEmail)
import Infra.Db.Receipt (insertReceipt)
import Infra.Claude.Client (callClaudeWithSystem)
import Agents.Concierge.Classifier (classifyActivity)
import Services.PeopleResolver (resolvePersonForActivity)
import Services.Router (routeActivity)

--------------------------------------------------------------------------------
-- Tool Call Types
--------------------------------------------------------------------------------

-- Tool calls concierge can output
data ConciergeToolCall
  = UpdateActivities [Text] ActivityUpdates
  | QueryActivities ActivityFilter
  | QueryPeople PeopleFilter
  deriving (Show, Eq)

data ActivityUpdates = ActivityUpdates
  { updatesStatus :: Maybe ActivityStatus
  , updatesClassification :: Maybe PartialClassification
  } deriving (Show, Eq, Generic)

-- Partial classification for updates (all fields optional)
data PartialClassification = PartialClassification
  { partialActivityType :: Maybe Text
  , partialUrgency :: Maybe Text
  , partialAutonomyTier :: Maybe Int
  , partialConfidence :: Maybe Double
  , partialPersonas :: Maybe [Text]
  , partialReasoning :: Maybe Text
  , partialSuggestedActions :: Maybe [Text]
  , partialOptionFraming :: Maybe Text
  } deriving (Show, Eq, Generic)

data ActivityFilter = ActivityFilter
  { filterStatus :: Maybe Text
  , filterLimit :: Maybe Int
  , filterSince :: Maybe UTCTime
  , filterBefore :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

data PeopleFilter = PeopleFilter
  { peopleEmail :: Maybe Text
  , peopleSearch :: Maybe Text
  , peopleLimit :: Maybe Int
  } deriving (Show, Eq, Generic)

-- Result from dispatcher
data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Instances
--------------------------------------------------------------------------------

instance FromJSON ConciergeToolCall where
  parseJSON = withObject "ConciergeToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "update_activities" -> do
        ids <- v .: "activity_ids"
        updates <- v .: "updates"
        pure $ UpdateActivities ids updates
      "query_activities" -> QueryActivities <$> parseJSON (Object v)
      "query_people" -> QueryPeople <$> parseJSON (Object v)
      _ -> fail $ "Unknown tool: " <> T.unpack tool

instance FromJSON ActivityUpdates where
  parseJSON = withObject "ActivityUpdates" $ \v -> ActivityUpdates
    <$> v .:? "status"
    <*> v .:? "classification"

instance FromJSON PartialClassification where
  parseJSON = withObject "PartialClassification" $ \v -> PartialClassification
    <$> v .:? "activity_type"
    <*> v .:? "urgency"
    <*> v .:? "autonomy_tier"
    <*> v .:? "confidence"
    <*> v .:? "personas"
    <*> v .:? "reasoning"
    <*> v .:? "suggested_actions"
    <*> v .:? "option_framing"

instance FromJSON ActivityFilter where
  parseJSON = withObject "ActivityFilter" $ \v -> ActivityFilter
    <$> v .:? "status"
    <*> v .:? "limit"
    <*> v .:? "since"
    <*> v .:? "before"

instance FromJSON PeopleFilter where
  parseJSON = withObject "PeopleFilter" $ \v -> PeopleFilter
    <$> v .:? "email"
    <*> v .:? "search"
    <*> v .:? "limit"

instance ToJSON ToolResult where
  toJSON (ToolSuccess v) = object ["success" .= True, "data" .= v]
  toJSON (ToolError err) = object ["success" .= False, "error" .= err]

--------------------------------------------------------------------------------
-- Tool Dispatcher
--------------------------------------------------------------------------------

executeToolCall :: ConciergeToolCall -> App ToolResult
executeToolCall (UpdateActivities ids updates) = do
  -- Get activities by ID
  activities <- fmap (mapMaybe id) $ forM ids $ \aid ->
    getActivity (EntityId aid)

  -- Apply updates
  forM_ activities $ \activity -> do
    -- Update status if provided
    case updatesStatus updates of
      Just newStatus -> updateActivityStatus (activityId activity) newStatus
      Nothing -> pure ()

    -- Update classification if provided
    case updatesClassification updates of
      Just partial -> do
        -- Build full classification from existing + partial
        -- For now, just update autonomy tier if provided (most common use case)
        case partialAutonomyTier partial of
          Just tier -> do
            let _reasoning = fromMaybe "Updated via chat" (partialReasoning partial)
            liftIO $ putStrLn $ "Updating activity " <> show (activityId activity) <> " tier to " <> show tier
            -- TODO: Implement partial classification updates in DB layer
            pure ()
          Nothing -> pure ()
      Nothing -> pure ()

  pure $ ToolSuccess $ object
    [ "updated_count" .= length activities
    , "activity_ids" .= ids
    ]

executeToolCall (QueryActivities filt) = do
  let limit = fromMaybe 20 (filterLimit filt)
  let mStatus = case filterStatus filt of
        Just "quarantined" -> Just Activity.Quarantined
        Just "surfaced" -> Just Activity.Surfaced
        Just "pending" -> Just Activity.Pending
        Just "needs_review" -> Just Activity.NeedsReview
        Just "processed" -> Just Activity.Processed
        Just "archived" -> Just Activity.Archived
        _ -> Nothing
  -- Use filtered query if date filters present, otherwise simple status query
  activities <- case (filterSince filt, filterBefore filt) of
    (Nothing, Nothing) -> case mStatus of
      Just st -> getActivitiesByStatus st limit
      Nothing -> getRecentActivities limit
    _ -> getActivitiesFiltered mStatus (filterSince filt) (filterBefore filt) limit

  pure $ ToolSuccess $ object
    [ "activities" .= map activityToJson activities
    , "count" .= length activities
    ]

executeToolCall (QueryPeople filt) = do
  let limit = fromMaybe 20 (peopleLimit filt)
  people <- case peopleEmail filt of
    Just email -> do
      mPerson <- getPersonByEmail email
      pure $ maybe [] (:[]) mPerson
    Nothing -> case peopleSearch filt of
      Just search -> searchPeople search limit
      Nothing -> searchPeople "" limit

  pure $ ToolSuccess $ object
    [ "people" .= map personToJson people
    , "count" .= length people
    ]

activityToJson :: Activity -> Value
activityToJson a = object
  [ "id" .= unEntityId (activityId a)
  , "title" .= activityTitle a
  , "status" .= activityStatus a
  , "sender_email" .= activitySenderEmail a
  , "summary" .= activitySummary a
  , "autonomy_tier" .= activityAutonomyTier a
  , "urgency" .= activityUrgency a
  ]

personToJson :: Person -> Value
personToJson p = object
  [ "id" .= unEntityId (personId p)
  , "email" .= personEmail p
  , "display_name" .= personDisplayName p
  ]

--------------------------------------------------------------------------------
-- Chat Handler
--------------------------------------------------------------------------------

-- Response from LLM that may include a tool call
data ChatResponse = ChatResponse
  { responseMessage :: Text
  , responseToolCall :: Maybe ConciergeToolCall
  } deriving (Show, Eq)

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v -> ChatResponse
    <$> v .: "message"
    <*> v .:? "tool_call"

handleChat :: Text -> App (Either Text Text)
handleChat query = do
  -- Assemble context
  calendar <- getTodaysCalendarEvents
  quarantined <- getActivitiesByStatus Activity.Quarantined 100
  surfaced <- getActivitiesByStatus Activity.Surfaced 100
  needsReview <- getActivitiesByStatus Activity.NeedsReview 100

  let systemPrompt = buildChatPrompt calendar quarantined surfaced needsReview

  claudeCfg <- asks (claude . config)
  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    (model claudeCfg)
    systemPrompt
    query

  case result of
    Left err -> pure $ Left err
    Right response -> do
      -- Log the conversation
      _ <- insertConversation query response
      -- Parse response
      case parseChatResponse response of
        Left err -> pure $ Left err
        Right chatResp -> do
          -- Execute tool call if present
          case responseToolCall chatResp of
            Nothing -> pure $ Right (responseMessage chatResp)
            Just toolCall -> do
              toolResult <- executeToolCall toolCall
              case toolResult of
                ToolSuccess _ -> pure $ Right (responseMessage chatResp)
                ToolError err -> pure $ Left err

buildChatPrompt :: [Activity] -> [Activity] -> [Activity] -> [Activity] -> Text
buildChatPrompt calendar quarantined surfaced needsReview = T.unlines
  [ "You are Wisp, a personal assistant. You present options, never demands."
  , ""
  , "## Response Format"
  , "IMPORTANT: Respond with ONLY valid JSON, no other text. No prose before or after."
  , ""
  , "{\"message\": \"Your response\", \"tool_call\": null}"
  , ""
  , "Or with a tool call:"
  , ""
  , "{\"message\": \"Your response\", \"tool_call\": {\"tool\": \"...\", ...}}"
  , ""
  , "## Available Tools"
  , ""
  , "### update_activities - Update status or classification"
  , "```json"
  , "{\"tool\": \"update_activities\", \"activity_ids\": [\"id1\"], \"updates\": {\"status\": \"processed\"}}"
  , "```"
  , "Status values: pending, needs_review, quarantined, surfaced, processed, archived"
  , ""
  , "### query_activities - Fetch activities"
  , "Filters: status, limit, since (ISO8601), before (ISO8601)"
  , "```json"
  , "{\"tool\": \"query_activities\", \"status\": \"quarantined\", \"limit\": 10}"
  , "{\"tool\": \"query_activities\", \"before\": \"2024-12-01T00:00:00Z\", \"limit\": 50}"
  , "```"
  , ""
  , "### query_people - Look up contacts"
  , "```json"
  , "{\"tool\": \"query_people\", \"email\": \"someone@example.com\"}"
  , "```"
  , ""
  , "## Style"
  , "- Present choices as options: \"You might want to...\" or \"Here are some options:\""
  , "- Never use imperatives like \"You should\" or \"Don't forget\""
  , "- Be concise and helpful"
  , ""
  , "## For Quarantined Items"
  , "When helping with quarantined items, present 2-3 classification options:"
  , "A) What it might be → suggested tier"
  , "B) Alternative interpretation → suggested tier"
  , "Let the user choose, then use update_activities to apply."
  , ""
  , "## Current State"
  , ""
  , "### Quarantined (" <> T.pack (show (length quarantined)) <> " items)"
  , formatActivitiesForPrompt quarantined
  , ""
  , "### Surfaced (" <> T.pack (show (length surfaced)) <> " items)"
  , formatActivitiesForPrompt surfaced
  , ""
  , "### Needs Review (" <> T.pack (show (length needsReview)) <> " items)"
  , formatActivitiesForPrompt needsReview
  , ""
  , "### Today's Calendar"
  , formatCalendarForPrompt calendar
  ]

formatActivitiesForPrompt :: [Activity] -> Text
formatActivitiesForPrompt [] = "(none)"
formatActivitiesForPrompt activities = T.unlines $ map formatOne (take 20 activities)
  where
    formatOne a = "- [" <> unEntityId (activityId a) <> "] "
      <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\s -> " (from: " <> s <> ")") (activitySenderEmail a)
      <> maybe "" (\s -> " - " <> s) (activitySummary a)

formatCalendarForPrompt :: [Activity] -> Text
formatCalendarForPrompt [] = "No events today."
formatCalendarForPrompt events = T.unlines $ map formatOne events
  where
    formatOne a = "- [" <> unEntityId (activityId a) <> "] "
      <> fromMaybe "(no title)" (activityTitle a)

parseChatResponse :: Text -> Either Text ChatResponse
parseChatResponse raw =
  let extracted = extractJson raw
      jsonBytes = BL.fromStrict (encodeUtf8 extracted)
  in case decode jsonBytes of
    Just resp -> Right resp
    Nothing -> Left $ "Failed to parse chat response: " <> T.take 200 raw

-- Extract JSON object from text that may contain prose before/after
-- Looks for outermost { } pair, handling nested braces
extractJson :: Text -> Text
extractJson t =
  let stripped = stripCodeBlock t
      -- Find the first { and last }
      startIdx = T.findIndex (== '{') stripped
      endIdx = findLastIndex (== '}') stripped
  in case (startIdx, endIdx) of
    (Just s, Just e) | e >= s -> T.drop s $ T.take (e + 1) stripped
    _ -> stripped  -- Fall back to original if no braces found

-- Find the last index of a character
findLastIndex :: (Char -> Bool) -> Text -> Maybe Int
findLastIndex p t =
  let len = T.length t
      indices = [i | i <- [0..len-1], p (T.index t i)]
  in if null indices then Nothing else Just (last indices)

-- Strip ```json ... ``` wrapper if present
stripCodeBlock :: Text -> Text
stripCodeBlock t =
  let lines' = T.lines t
      withoutStart = case lines' of
        (l:rest) | "```" `T.isPrefixOf` l -> rest
        other -> other
      withoutEnd = case reverse withoutStart of
        (l:rest) | l == "```" -> reverse rest
        other -> reverse other
  in T.unlines withoutEnd

--------------------------------------------------------------------------------
-- Deterministic Flows
--------------------------------------------------------------------------------

-- Process all pending activities through the pipeline
classifyAllPending :: Int -> App [(Text, Either Text ActivityStatus)]
classifyAllPending limit = do
  activities <- getActivitiesByStatus Activity.Pending limit
  forM activities $ \activity -> do
    result <- classifyPending activity
    let actId = T.pack $ show (activityId activity)
    pure (actId, result)

-- Process a single activity through classify -> resolve -> route
classifyPending :: Activity -> App (Either Text ActivityStatus)
classifyPending activity = do
  -- Step 1: Classify
  liftIO $ putStrLn $ "Classifying activity: " <> show (activityId activity)
  classifyResult <- classifyActivity activity
  case classifyResult of
    Left err -> do
      liftIO $ putStrLn $ "Classification failed: " <> T.unpack err
      -- Create failure receipt
      void $ insertReceipt NewReceipt
        { newReceiptActivityId = activityId activity
        , newReceiptActionTaken = ClassificationFailed
        , newReceiptActionDetail = Just err
        , newReceiptConfidence = Nothing
        }
      pure $ Left err
    Right classification -> do
      liftIO $ putStrLn $ "Classification: tier=" <> show (classificationAutonomyTier classification)
                       <> ", confidence=" <> show (classificationConfidence classification)

      -- Create classification receipt
      void $ insertReceipt NewReceipt
        { newReceiptActivityId = activityId activity
        , newReceiptActionTaken = Classified
        , newReceiptActionDetail = Just (classificationReasoning classification)
        , newReceiptConfidence = Just (classificationConfidence classification)
        }

      -- Step 2: Resolve person
      mPerson <- resolvePersonForActivity activity
      let mPersonId = fmap personId mPerson
      liftIO $ putStrLn $ "Resolved person: " <> show (fmap personEmail mPerson)

      -- Step 3: Update activity with classification
      updateActivityClassification (activityId activity) classification mPersonId

      -- Step 4: Route
      newStatus <- routeActivity activity classification
      liftIO $ putStrLn $ "Routed to status: " <> show newStatus

      -- Create routing receipt
      void $ insertReceipt NewReceipt
        { newReceiptActivityId = activityId activity
        , newReceiptActionTaken = statusToReceiptAction newStatus
        , newReceiptActionDetail = Nothing
        , newReceiptConfidence = Nothing
        }

      pure $ Right newStatus

-- Convert ActivityStatus to appropriate ReceiptAction
statusToReceiptAction :: ActivityStatus -> ReceiptAction
statusToReceiptAction Activity.Pending = Classified  -- shouldn't happen
statusToReceiptAction Activity.NeedsReview = RoutedToNeedsReview
statusToReceiptAction Activity.Quarantined = RoutedToQuarantined
statusToReceiptAction Activity.Surfaced = RoutedToSurfaced
statusToReceiptAction Activity.Processed = RoutedToProcessed
statusToReceiptAction Activity.Archived = RoutedToArchived
