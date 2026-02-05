module Agents.Insights
  ( -- Decision flows
    handleChat
    -- Types
  , InsightsToolCall(..)
  , SearchQuery(..)
  , SummaryQuery(..)
  , PeopleQuery(..)
  , ToolResult(..)
    -- Dispatcher
  , executeToolCall
    -- Agent metadata
  , agentInfo
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), decode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import qualified Data.Time.Format as TF
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
import App.Monad (App, Env(..))
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..))
import Domain.Person (Person(..))
import Domain.Id (unEntityId)
import Infra.Db.Activity (searchActivities, getActivitySummaryStats, getRecentActivities, insertConversation)
import Infra.Db.Person (getAllPeople, getImportantPeople, searchPeople)
import Infra.Claude.Client (callClaudeWithSystem)
import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))
import Domain.Chat (ChatMessage(..), ChatResponse)
import qualified Domain.Chat as Chat

--------------------------------------------------------------------------------
-- Timezone Loading
--------------------------------------------------------------------------------

loadTimezone :: Text -> IO (Maybe TZ)
loadTimezone tzName = do
  result <- try $ loadSystemTZ (T.unpack tzName)
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right tz -> pure (Just tz)

--------------------------------------------------------------------------------
-- Tool Call Types
--------------------------------------------------------------------------------

data InsightsToolCall
  = SearchActivities SearchQuery
  | GetSummary SummaryQuery
  | GetPeopleInsights PeopleQuery
  deriving (Show, Eq)

data SearchQuery = SearchQuery
  { searchTerm :: Text
  , searchLimit :: Maybe Int
  } deriving (Show, Eq)

data SummaryQuery = SummaryQuery
  { summaryHours :: Maybe Int
  } deriving (Show, Eq)

data PeopleQuery = PeopleQuery
  { peopleSearch :: Maybe Text
  , peopleImportantOnly :: Maybe Bool
  } deriving (Show, Eq)

data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Instances
--------------------------------------------------------------------------------

instance FromJSON InsightsToolCall where
  parseJSON = withObject "InsightsToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "search_activities" -> SearchActivities <$> parseJSON (Object v)
      "get_summary" -> GetSummary <$> parseJSON (Object v)
      "get_people_insights" -> GetPeopleInsights <$> parseJSON (Object v)
      _ -> fail $ "Unknown tool: " <> T.unpack tool

instance FromJSON SearchQuery where
  parseJSON = withObject "SearchQuery" $ \v -> SearchQuery
    <$> v .: "query"
    <*> v .:? "limit"

instance FromJSON SummaryQuery where
  parseJSON = withObject "SummaryQuery" $ \v -> SummaryQuery
    <$> v .:? "hours"

instance FromJSON PeopleQuery where
  parseJSON = withObject "PeopleQuery" $ \v -> PeopleQuery
    <$> v .:? "search"
    <*> v .:? "important_only"

instance ToJSON ToolResult where
  toJSON (ToolSuccess v) = object ["success" .= True, "data" .= v]
  toJSON (ToolError err) = object ["success" .= False, "error" .= err]

--------------------------------------------------------------------------------
-- Tool Dispatcher
--------------------------------------------------------------------------------

executeToolCall :: Maybe TZ -> InsightsToolCall -> App ToolResult
executeToolCall mTz (SearchActivities q) = do
  let limit = fromMaybe 20 (searchLimit q)
  results <- searchActivities (searchTerm q) limit
  pure $ ToolSuccess $ object
    [ "activities" .= map (activityToJson mTz) results
    , "count" .= length results
    , "query" .= searchTerm q
    ]

executeToolCall _mTz (GetSummary q) = do
  let hours = fromMaybe 24 (summaryHours q)
  activities <- getRecentActivities hours
  stats <- getActivitySummaryStats
  pure $ ToolSuccess $ object
    [ "recent_count" .= length activities
    , "hours" .= hours
    , "stats" .= map statsToJson stats
    ]

executeToolCall _mTz (GetPeopleInsights q) = do
  people <- case (peopleSearch q, peopleImportantOnly q) of
    (Just term, _) -> searchPeople term 50
    (Nothing, Just True) -> getImportantPeople
    (Nothing, _) -> getAllPeople
  pure $ ToolSuccess $ object
    [ "people" .= map personToJson people
    , "count" .= length people
    ]

activityToJson :: Maybe TZ -> Activity -> Value
activityToJson mTz a = object
  [ "id" .= unEntityId (activityId a)
  , "source" .= activitySource a
  , "title" .= activityTitle a
  , "summary" .= activitySummary a
  , "status" .= activityStatus a
  , "sender_email" .= activitySenderEmail a
  , "starts_at" .= fmap (formatTime mTz) (activityStartsAt a)
  , "ends_at" .= fmap (formatTime mTz) (activityEndsAt a)
  , "created_at" .= formatTime mTz (activityCreatedAt a)
  , "type" .= activityType a
  , "urgency" .= activityUrgency a
  , "person_id" .= fmap unEntityId (activityPersonId a)
  ]

personToJson :: Person -> Value
personToJson p = object
  [ "id" .= unEntityId (personId p)
  , "email" .= personEmail p
  , "display_name" .= personDisplayName p
  , "personas" .= personPersonas p
  , "relationship" .= personRelationship p
  , "organisation" .= personOrganisation p
  , "first_contact" .= personFirstContact p
  , "last_contact" .= personLastContact p
  , "contact_count" .= personContactCount p
  ]

statsToJson :: (Text, Text, Int) -> Value
statsToJson (source, status, count) = object
  [ "source" .= source
  , "status" .= status
  , "count" .= count
  ]

formatTime :: Maybe TZ -> UTCTime -> Text
formatTime Nothing utc = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" utc
formatTime (Just tz) utc =
  let localTime = utcToLocalTimeTZ tz utc
  in T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" localTime

--------------------------------------------------------------------------------
-- Chat Handler
--------------------------------------------------------------------------------

data LLMResponse = LLMResponse
  { llmMessage :: Text
  , llmToolCall :: Maybe InsightsToolCall
  } deriving (Show, Eq)

instance FromJSON LLMResponse where
  parseJSON = withObject "LLMResponse" $ \v -> LLMResponse
    <$> v .: "message"
    <*> v .:? "tool_call"

handleChat :: [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleChat messages mTzName = do
  let userMessages = [m | m <- messages, messageRole m == "user"]
  case userMessages of
    [] -> pure $ Left "No user message provided"
    _ -> do
      let query = messageContent (last userMessages)

      mTz <- case mTzName of
        Nothing -> pure Nothing
        Just tzName -> liftIO $ loadTimezone tzName

      -- Get context for the agent
      recentActivities <- getRecentActivities 24
      stats <- getActivitySummaryStats

      let systemPrompt = buildChatPrompt mTz recentActivities stats
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
          _ <- insertConversation query response
          case parseLLMResponse response of
            Left err -> pure $ Left err
            Right llmResp -> do
              case llmToolCall llmResp of
                Nothing -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                Just toolCall -> do
                  toolResult <- executeToolCall mTz toolCall
                  case toolResult of
                    ToolSuccess _ -> pure $ Right $ Chat.ChatResponse (llmMessage llmResp) Nothing
                    ToolError err -> pure $ Left err

buildConversationPrompt :: [ChatMessage] -> Text
buildConversationPrompt msgs = T.unlines
  [ role <> ": " <> messageContent m
  | m <- msgs
  , let role = case messageRole m of
          "user" -> "User"
          "assistant" -> fromMaybe "Assistant" (messageAgent m)
          "tool" -> "Tool Result"
          r -> r
  ]

buildChatPrompt :: Maybe TZ -> [Activity] -> [(Text, Text, Int)] -> Text
buildChatPrompt mTz recentActivities stats = T.unlines
  [ "You are Wisp's insights agent. You help with searching activities, viewing summaries, and exploring people relationships."
  , ""
  , "## Response Format"
  , "IMPORTANT: Respond with ONLY valid JSON, no other text."
  , ""
  , "{\"message\": \"Your response\", \"tool_call\": null}"
  , ""
  , "Or with a tool call:"
  , ""
  , "{\"message\": \"Your response\", \"tool_call\": {\"tool\": \"...\", ...}}"
  , ""
  , "## Available Tools"
  , ""
  , "### search_activities - Search through activities"
  , "```json"
  , "{\"tool\": \"search_activities\", \"query\": \"meeting\", \"limit\": 20}"
  , "```"
  , ""
  , "### get_summary - Get activity statistics and recent activity summary"
  , "```json"
  , "{\"tool\": \"get_summary\", \"hours\": 24}"
  , "```"
  , ""
  , "### get_people_insights - View people and their interactions"
  , "```json"
  , "{\"tool\": \"get_people_insights\", \"search\": \"john\", \"important_only\": false}"
  , "{\"tool\": \"get_people_insights\", \"important_only\": true}"
  , "```"
  , ""
  , "## Style"
  , "- Present information in a clear, organized way"
  , "- Highlight patterns and interesting insights"
  , "- Be concise but thorough"
  , "- When showing activity data, focus on what's most relevant to the user's question"
  , ""
  , "## Current Context"
  , ""
  , "### Recent Activity (last 24h): " <> T.pack (show (length recentActivities)) <> " items"
  , formatRecentActivities mTz (take 10 recentActivities)
  , ""
  , "### Activity Summary"
  , formatStats stats
  ]

formatRecentActivities :: Maybe TZ -> [Activity] -> Text
formatRecentActivities _ [] = "(no recent activity)"
formatRecentActivities _ activities = T.unlines $ map formatOne activities
  where
    formatOne a = "- " <> formatSource (activitySource a) <> ": " <>
                  fromMaybe "(untitled)" (activityTitle a) <> " [" <>
                  T.pack (show (activityStatus a)) <> "]"
    formatSource src = "[" <> T.pack (show src) <> "]"

formatStats :: [(Text, Text, Int)] -> Text
formatStats [] = "(no stats available)"
formatStats stats = T.unlines $ map formatStat stats
  where
    formatStat (source, status, count) = "- " <> source <> "/" <> status <> ": " <> T.pack (show count)

parseLLMResponse :: Text -> Either Text LLMResponse
parseLLMResponse raw =
  let extracted = extractJson raw
      jsonBytes = BL.fromStrict (encodeUtf8 extracted)
  in case decode jsonBytes of
    Just resp -> Right resp
    Nothing -> Left $ "Failed to parse response: " <> T.take 200 raw

extractJson :: Text -> Text
extractJson t =
  let stripped = stripCodeBlock t
      startIdx = T.findIndex (== '{') stripped
      endIdx = findLastIndex (== '}') stripped
  in case (startIdx, endIdx) of
    (Just s, Just e) | e >= s -> T.drop s $ T.take (e + 1) stripped
    _ -> stripped

findLastIndex :: (Char -> Bool) -> Text -> Maybe Int
findLastIndex p t =
  let len = T.length t
      indices = [i | i <- [0..len-1], p (T.index t i)]
  in if null indices then Nothing else Just (last indices)

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
-- Agent Metadata
--------------------------------------------------------------------------------

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "wisp/insights"
  , agentDescription = "Retrieval, summaries, feedback clustering"
  , agentTools =
      [ ToolInfo "search_activities" Decision
      , ToolInfo "get_summary" Decision
      , ToolInfo "get_people_insights" Decision
      ]
  , agentWorkflows = ["feedback-cluster", "generate-summary"]
  , agentImplemented = True
  }
