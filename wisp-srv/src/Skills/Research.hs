-- | Research Skill
-- Structured research workflow with parallel sub-agents.
module Skills.Research
  ( -- Types
    ResearchToolCall(..)
  , CreatePlanQuery(..)
  , WebSearchQuery(..)
  , WriteFindingQuery(..)
  , CompleteResearchQuery(..)
  , ToolResult(..)
    -- Execution
  , executeToolCall
    -- Utilities
  , generateSessionId
    -- Agent metadata
  , agentInfo
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))
import Domain.Activity (Activity(..))
import Domain.Id (EntityId, newEntityId, unEntityId)
import App.Monad (App)
import Infra.Db.Activity (insertNote, searchActivities)
import qualified Services.Tavily as Tavily

--------------------------------------------------------------------------------
-- Tool Call Types
--------------------------------------------------------------------------------

data ResearchToolCall
  = CreateResearchPlan CreatePlanQuery
  | WebSearch WebSearchQuery
  | WriteFinding WriteFindingQuery
  | CompleteResearch CompleteResearchQuery
  deriving (Show, Eq)

data CreatePlanQuery = CreatePlanQuery
  { planTopic :: Text          -- Research topic
  , planTasks :: [Text]        -- List of research tasks
  } deriving (Show, Eq)

data WebSearchQuery = WebSearchQuery
  { webSearchQuery :: Text     -- Search query
  , webSearchLimit :: Maybe Int  -- Max results (default: 5)
  } deriving (Show, Eq)

data WriteFindingQuery = WriteFindingQuery
  { findingSessionId :: Text   -- Session ID from create_research_plan
  , findingTitle :: Text       -- Finding title
  , findingContent :: Text     -- Finding content
  } deriving (Show, Eq)

data CompleteResearchQuery = CompleteResearchQuery
  { completeSessionId :: Text  -- Session ID to summarize
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Tool Result
--------------------------------------------------------------------------------

data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)

instance ToJSON ToolResult where
  toJSON (ToolSuccess v) = object ["success" .= True, "data" .= v]
  toJSON (ToolError err) = object ["success" .= False, "error" .= err]

--------------------------------------------------------------------------------
-- Session ID Generation
--------------------------------------------------------------------------------

-- | Generate a unique session ID in format: research-YYYYMMDD-abc123
generateSessionId :: App Text
generateSessionId = do
  now <- liftIO getCurrentTime
  let dateStr = formatTime defaultTimeLocale "%Y%m%d" now
  shortId <- liftIO newEntityId
  let idStr = T.take 6 (unEntityId shortId)
  pure $ "research-" <> T.pack dateStr <> "-" <> idStr

--------------------------------------------------------------------------------
-- JSON Instances (FromJSON)
--------------------------------------------------------------------------------

instance FromJSON ResearchToolCall where
  parseJSON = withObject "ResearchToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "create_research_plan" -> CreateResearchPlan <$> parseJSON (Object v)
      "web_search"           -> WebSearch <$> parseJSON (Object v)
      "write_finding"        -> WriteFinding <$> parseJSON (Object v)
      "complete_research"    -> CompleteResearch <$> parseJSON (Object v)
      _ -> fail $ "Unknown Research tool: " <> T.unpack tool

instance FromJSON CreatePlanQuery where
  parseJSON = withObject "CreatePlanQuery" $ \v -> CreatePlanQuery
    <$> v .: "topic"
    <*> v .: "tasks"

instance FromJSON WebSearchQuery where
  parseJSON = withObject "WebSearchQuery" $ \v -> WebSearchQuery
    <$> v .: "query"
    <*> v .:? "limit"

instance FromJSON WriteFindingQuery where
  parseJSON = withObject "WriteFindingQuery" $ \v -> WriteFindingQuery
    <$> v .: "session_id"
    <*> v .: "title"
    <*> v .: "content"

instance FromJSON CompleteResearchQuery where
  parseJSON = withObject "CompleteResearchQuery" $ \v -> CompleteResearchQuery
    <$> v .: "session_id"

--------------------------------------------------------------------------------
-- JSON Instances (ToJSON)
--------------------------------------------------------------------------------

instance ToJSON ResearchToolCall where
  toJSON (CreateResearchPlan q) = object
    [ "tool" .= ("create_research_plan" :: Text)
    , "topic" .= planTopic q
    , "tasks" .= planTasks q
    ]
  toJSON (WebSearch q) = object
    [ "tool" .= ("web_search" :: Text)
    , "query" .= webSearchQuery q
    , "limit" .= webSearchLimit q
    ]
  toJSON (WriteFinding q) = object
    [ "tool" .= ("write_finding" :: Text)
    , "session_id" .= findingSessionId q
    , "title" .= findingTitle q
    , "content" .= findingContent q
    ]
  toJSON (CompleteResearch q) = object
    [ "tool" .= ("complete_research" :: Text)
    , "session_id" .= completeSessionId q
    ]

instance ToJSON CreatePlanQuery where
  toJSON CreatePlanQuery {..} = object
    [ "topic" .= planTopic
    , "tasks" .= planTasks
    ]

instance ToJSON WebSearchQuery where
  toJSON WebSearchQuery {..} = object
    [ "query" .= webSearchQuery
    , "limit" .= webSearchLimit
    ]

instance ToJSON WriteFindingQuery where
  toJSON WriteFindingQuery {..} = object
    [ "session_id" .= findingSessionId
    , "title" .= findingTitle
    , "content" .= findingContent
    ]

instance ToJSON CompleteResearchQuery where
  toJSON CompleteResearchQuery {..} = object
    [ "session_id" .= completeSessionId
    ]

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Execute a Research tool call
-- accountId is needed for writing notes
executeToolCall :: EntityId -> ResearchToolCall -> App ToolResult
executeToolCall accountId call = case call of
  CreateResearchPlan q -> do
    sessionId <- generateSessionId
    pure $ ToolSuccess $ object
      [ "session_id" .= sessionId
      , "topic" .= planTopic q
      , "tasks" .= planTasks q
      , "instructions" .= ("Use web_search and write_finding for each task, then complete_research" :: Text)
      ]

  WebSearch q -> do
    let limit = fromMaybe 5 (webSearchLimit q)
    result <- Tavily.search (webSearchQuery q) limit
    case result of
      Left err -> pure $ ToolError err
      Right results -> pure $ ToolSuccess $ object
        [ "query" .= webSearchQuery q
        , "results" .= map toJSON results
        , "count" .= length results
        ]

  WriteFinding q -> do
    let tags = [sessionTag (findingSessionId q), "research:finding"]
        content = "# " <> findingTitle q <> "\n\n" <> findingContent q
        rawMeta = object ["origin" .= ("research" :: Text), "session_id" .= findingSessionId q]
    mId <- insertNote accountId content tags rawMeta
    case mId of
      Just noteId -> pure $ ToolSuccess $ object
        [ "note_id" .= noteId
        , "session_id" .= findingSessionId q
        , "status" .= ("finding_saved" :: Text)
        ]
      Nothing -> pure $ ToolError "Failed to save finding"

  CompleteResearch q -> do
    -- Search for all findings in this session
    let searchTerm = "research:" <> completeSessionId q
    findings <- searchActivities searchTerm 100
    let findingContents = [activitySummary a | a <- findings, hasTag (sessionTag (completeSessionId q)) a]

    -- Create summary note
    let summaryContent = T.unlines $
          [ "# Research Summary"
          , ""
          , "Session: " <> completeSessionId q
          , ""
          , "## Findings"
          , ""
          ] ++ map formatFinding (zip [1..] findingContents)

    let tags = [sessionTag (completeSessionId q), "research:summary"]
        rawMeta = object ["origin" .= ("research" :: Text), "session_id" .= completeSessionId q]

    mId <- insertNote accountId summaryContent tags rawMeta
    case mId of
      Just noteId -> pure $ ToolSuccess $ object
        [ "note_id" .= noteId
        , "session_id" .= completeSessionId q
        , "findings_count" .= length findingContents
        , "status" .= ("research_complete" :: Text)
        ]
      Nothing -> pure $ ToolError "Failed to save summary"

-- | Build session tag from session ID
sessionTag :: Text -> Text
sessionTag sessionId = "research:" <> sessionId

-- | Check if activity has a specific tag
hasTag :: Text -> Activity -> Bool
hasTag tag activity = tag `elem` activityTags activity

-- | Format a finding for the summary
formatFinding :: (Int, Maybe Text) -> Text
formatFinding (n, mContent) = case mContent of
  Just content -> T.pack (show n) <> ". " <> T.take 200 content <> "..."
  Nothing -> T.pack (show n) <> ". (no content)"

--------------------------------------------------------------------------------
-- Agent Metadata
--------------------------------------------------------------------------------

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "research"
  , agentDescription = "Structured research workflow with web search and knowledge synthesis"
  , agentTools =
      [ ToolInfo "create_research_plan" Decision
      , ToolInfo "web_search" Decision
      , ToolInfo "write_finding" Decision
      , ToolInfo "complete_research" Decision
      ]
  , agentWorkflows = ["research", "information-gathering", "synthesis"]
  , agentImplemented = True
  }
