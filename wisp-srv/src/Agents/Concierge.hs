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
    -- Agent metadata
  , agentInfo
    -- Context-aware chat
  , handleChatWithContext
  ) where

import Control.Exception (try, SomeException)
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
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
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
import Agents.Run (RunContext(..), callClaudeLogged, logToolRequest, logToolSuccess, logToolFailure)
import Agents.Concierge.Classifier (classifyActivity)
import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))
import Domain.Chat (ChatMessage(..), ChatResponse)
import qualified Domain.Chat as Chat
import Services.PeopleResolver (resolvePersonForActivity)
import Services.Router (routeActivity)

--------------------------------------------------------------------------------
-- Timezone Loading
--------------------------------------------------------------------------------

-- | Load timezone from IANA name, returning Nothing if invalid
loadTimezone :: Text -> IO (Maybe TZ)
loadTimezone tzName = do
  result <- try $ loadSystemTZ (T.unpack tzName)
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right tz -> pure (Just tz)

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

instance ToJSON ConciergeToolCall where
  toJSON (UpdateActivities ids updates) = object
    [ "tool" .= ("update_activities" :: Text)
    , "activity_ids" .= ids
    , "updates" .= updates
    ]
  toJSON (QueryActivities filt) = object
    [ "tool" .= ("query_activities" :: Text)
    , "status" .= filterStatus filt
    , "limit" .= filterLimit filt
    , "since" .= filterSince filt
    , "before" .= filterBefore filt
    ]
  toJSON (QueryPeople filt) = object
    [ "tool" .= ("query_people" :: Text)
    , "email" .= peopleEmail filt
    , "search" .= peopleSearch filt
    , "limit" .= peopleLimit filt
    ]

instance ToJSON ActivityUpdates where
  toJSON ActivityUpdates {..} = object
    [ "status" .= updatesStatus
    , "classification" .= updatesClassification
    ]

instance ToJSON PartialClassification where
  toJSON PartialClassification {..} = object
    [ "activity_type" .= partialActivityType
    , "urgency" .= partialUrgency
    , "autonomy_tier" .= partialAutonomyTier
    , "confidence" .= partialConfidence
    , "personas" .= partialPersonas
    , "reasoning" .= partialReasoning
    , "suggested_actions" .= partialSuggestedActions
    , "option_framing" .= partialOptionFraming
    ]

instance ToJSON ActivityFilter where
  toJSON ActivityFilter {..} = object
    [ "status" .= filterStatus
    , "limit" .= filterLimit
    , "since" .= filterSince
    , "before" .= filterBefore
    ]

instance ToJSON PeopleFilter where
  toJSON PeopleFilter {..} = object
    [ "email" .= peopleEmail
    , "search" .= peopleSearch
    , "limit" .= peopleLimit
    ]

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
    [ "activities" .= map (activityToJson Nothing) activities  -- Tool results stay in UTC
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

-- | Convert activity to JSON for agent context
-- If timezone is provided, converts UTC times to local time (no Z suffix)
activityToJson :: Maybe TZ -> Activity -> Value
activityToJson mTz a = object
  [ "id" .= unEntityId (activityId a)
  , "title" .= activityTitle a
  , "status" .= activityStatus a
  , "sender_email" .= activitySenderEmail a
  , "summary" .= activitySummary a
  , "autonomy_tier" .= activityAutonomyTier a
  , "urgency" .= activityUrgency a
  , "starts_at" .= fmap (formatTime mTz) (activityStartsAt a)
  , "ends_at" .= fmap (formatTime mTz) (activityEndsAt a)
  ]

-- | Format UTCTime, converting to local if timezone provided
formatTime :: Maybe TZ -> UTCTime -> Text
formatTime Nothing utc = T.pack $ show utc  -- UTC format with space
formatTime (Just tz) utc =
  let local = utcToLocalTimeTZ tz utc
  in T.pack $ show local  -- Local format without Z suffix

personToJson :: Person -> Value
personToJson p = object
  [ "id" .= unEntityId (personId p)
  , "email" .= personEmail p
  , "display_name" .= personDisplayName p
  ]

--------------------------------------------------------------------------------
-- Chat Handler
--------------------------------------------------------------------------------

-- Response from LLM that may include a tool call (internal type)
data LLMResponse = LLMResponse
  { llmMessage :: Text
  , llmToolCall :: Maybe ConciergeToolCall
  } deriving (Show, Eq)

instance FromJSON LLMResponse where
  parseJSON = withObject "LLMResponse" $ \v -> LLMResponse
    <$> v .: "message"
    <*> v .:? "tool_call"

handleChat :: [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChat messages mTzName = do
  -- Get the last user message
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      let query = messageContent (last userMessages)

      -- Load timezone if provided
      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Assemble context
      calendar <- getTodaysCalendarEvents
      quarantined <- getActivitiesByStatus Activity.Quarantined 100
      surfaced <- getActivitiesByStatus Activity.Surfaced 100
      needsReview <- getActivitiesByStatus Activity.NeedsReview 100

      let systemPrompt = buildChatPrompt mTz calendar quarantined surfaced needsReview
      -- Build conversation for Claude (include history)
      let conversationPrompt = buildConversationPrompt messages

      claudeCfg <- asks (claude . config)
      result <- liftIO $ callClaudeWithSystem
        (apiKey claudeCfg)
        (model claudeCfg)
        systemPrompt
        conversationPrompt

      case result of
        Left err -> pure $ Left err
        Right response -> do
          -- Log the conversation
          _ <- insertConversation query response
          -- Parse response
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              -- Execute tool call if present
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCall toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

-- | Handle chat with run context for event logging
handleChatWithContext :: RunContext -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChatWithContext ctx messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      -- Load timezone if provided
      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Assemble context
      calendar <- getTodaysCalendarEvents
      quarantined <- getActivitiesByStatus Activity.Quarantined 100
      surfaced <- getActivitiesByStatus Activity.Surfaced 100
      needsReview <- getActivitiesByStatus Activity.NeedsReview 100

      let systemPrompt = buildChatPrompt mTz calendar quarantined surfaced needsReview
      let conversationPrompt = buildConversationPrompt messages

      -- Call Claude with logging
      result <- callClaudeLogged ctx systemPrompt conversationPrompt

      case result of
        Left err -> pure $ Left err
        Right response -> do
          -- Parse response
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              -- Execute tool call if present
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCallLogged ctx toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

-- | Execute tool call with logging
executeToolCallLogged :: RunContext -> ConciergeToolCall -> App ToolResult
executeToolCallLogged ctx toolCall = do
  let toolName = case toolCall of
        UpdateActivities {} -> "update_activities"
        QueryActivities {} -> "query_activities"
        QueryPeople {} -> "query_people"

  -- Log tool request
  _ <- logToolRequest ctx toolName (toJSON toolCall)

  -- Execute the tool
  result <- executeToolCall toolCall

  -- Log result
  case result of
    ToolSuccess val -> do
      logToolSuccess ctx toolName val
      pure result
    ToolError err -> do
      logToolFailure ctx toolName err
      pure result

-- Build conversation string from message history
buildConversationPrompt :: [ChatMessage] -> Text
buildConversationPrompt msgs = T.unlines
  [ role <> ": " <> messageContent m
  | m <- msgs
  , let role = case messageRole m of
          "user" -> "User"
          "assistant" -> fromMaybe "Assistant" (messageAgent m)  -- Use agent name if available
          "tool" -> "Tool Result"
          r -> r
  ]

buildChatPrompt :: Maybe TZ -> [Activity] -> [Activity] -> [Activity] -> [Activity] -> Text
buildChatPrompt mTz calendar quarantined surfaced needsReview = T.unlines
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
  , "- All times shown are in the user's local timezone"
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
  , formatCalendarForPrompt mTz calendar
  ]

formatActivitiesForPrompt :: [Activity] -> Text
formatActivitiesForPrompt [] = "(none)"
formatActivitiesForPrompt activities = T.unlines $ map formatOne (take 20 activities)
  where
    formatOne a = "- [" <> unEntityId (activityId a) <> "] "
      <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\s -> " (from: " <> s <> ")") (activitySenderEmail a)
      <> maybe "" (\s -> " - " <> s) (activitySummary a)

formatCalendarForPrompt :: Maybe TZ -> [Activity] -> Text
formatCalendarForPrompt _ [] = "No events today."
formatCalendarForPrompt mTz events = T.unlines $ map formatOne events
  where
    formatOne a = "- [" <> unEntityId (activityId a) <> "] "
      <> formatEventTime mTz (activityStartsAt a) (activityEndsAt a)
      <> fromMaybe "(no title)" (activityTitle a)

    formatEventTime :: Maybe TZ -> Maybe UTCTime -> Maybe UTCTime -> Text
    formatEventTime _ Nothing _ = ""
    formatEventTime mtz (Just start) mEnd =
      let startStr = formatTimeShort mtz start
          endStr = maybe "" (\e -> "-" <> formatTimeShort mtz e) mEnd
      in startStr <> endStr <> " "

    formatTimeShort :: Maybe TZ -> UTCTime -> Text
    formatTimeShort Nothing utc =
      -- Extract HH:MM from UTC string
      let s = T.pack $ show utc
      in T.take 5 $ T.drop 11 s
    formatTimeShort (Just tz) utc =
      let local = utcToLocalTimeTZ tz utc
          s = T.pack $ show local
      in T.take 5 $ T.drop 11 s

parseLLMResponse :: Text -> Either Text LLMResponse
parseLLMResponse raw =
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

--------------------------------------------------------------------------------
-- Agent Metadata
--------------------------------------------------------------------------------

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "wisp/concierge"
  , agentDescription = "Intake, classification, routing, quarantine"
  , agentTools =
      [ ToolInfo "update_activities" Decision
      , ToolInfo "query_activities" Decision
      , ToolInfo "query_people" Decision
      ]
  , agentWorkflows = ["classify-pending", "route-activity", "quarantine-interview"]
  , agentImplemented = True
  }
