-- | Skill Registry
-- Maps skill names to their implementations.
-- Each skill provides tools and can fetch its own context.
module Skills.Registry
  ( Skill(..)
  , ToolDef(..)
  , SkillContext(..)
  , SkillToolResult(..)
  , getSkill
  , allSkillNames
  , emptySkillContext
  , executeSkillTool
  , baseTools
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result(..), Value(..), fromJSON, toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Zones (TZ, loadSystemTZ)
import Domain.Activity (Activity)
import Domain.Person (Person)
import Domain.Id (EntityId)
import App.Monad (App)
import qualified Skills.Concierge as Concierge
import qualified Skills.Scheduler as Scheduler
import qualified Skills.Insights as Insights
import qualified Skills.GitHub as GitHub
import qualified Skills.Research as Research

-- | Context that a skill can provide to the agent
data SkillContext = SkillContext
  { skillContextActivities :: [Activity]  -- ^ Relevant activities for this skill
  , skillContextPeople     :: [Person]    -- ^ Relevant people for this skill
  , skillContextExtra      :: Value       -- ^ Skill-specific additional data
  } deriving (Show)

-- | Empty skill context
emptySkillContext :: SkillContext
emptySkillContext = SkillContext [] [] (toJSON ())

-- | Tool definition with name and example JSON schema
data ToolDef = ToolDef
  { toolDefName   :: Text   -- ^ Tool name
  , toolDefSchema :: Text   -- ^ Example JSON showing parameters
  } deriving (Show, Eq)

-- | A skill provides tools and can fetch its own context
data Skill = Skill
  { skillName         :: Text              -- ^ Skill identifier (e.g. "concierge")
  , skillToolNames    :: [Text]            -- ^ Tool names this skill provides (for backwards compat)
  , skillTools        :: [ToolDef]         -- ^ Tool definitions with schemas
  , skillFetchContext :: App SkillContext  -- ^ How to get skill-specific context
  }

-- | Registry of available skill names
allSkillNames :: [Text]
allSkillNames = ["concierge", "scheduler", "insights", "github", "research"]

-- | Look up a skill by name
getSkill :: Text -> Maybe Skill
getSkill "concierge" = Just conciergeSkill
getSkill "scheduler" = Just schedulerSkill
getSkill "insights"  = Just insightsSkill
getSkill "github"    = Just githubSkill
getSkill "research"  = Just researchSkill
getSkill _           = Nothing

-- Skill definitions

conciergeSkill :: Skill
conciergeSkill = Skill
  { skillName = "concierge"
  , skillToolNames = ["update_activities", "query_activities", "query_people"]
  , skillTools =
      [ ToolDef "update_activities" "{\"tool\": \"update_activities\", \"activity_ids\": [\"id1\", \"id2\"], \"status\": \"processed\"}"
      , ToolDef "query_activities" "{\"tool\": \"query_activities\", \"status\": \"pending\", \"limit\": 10}"
      , ToolDef "query_people" "{\"tool\": \"query_people\", \"search\": \"name or email\", \"limit\": 10}"
      ]
  , skillFetchContext = pure emptySkillContext
  }

schedulerSkill :: Skill
schedulerSkill = Skill
  { skillName = "scheduler"
  , skillToolNames = ["query_calendar", "find_free_slots", "list_connected_accounts"]
  , skillTools =
      [ ToolDef "query_calendar" "{\"tool\": \"query_calendar\", \"days\": 7}"
      , ToolDef "find_free_slots" "{\"tool\": \"find_free_slots\", \"days\": 7, \"duration_minutes\": 60, \"start_hour\": 9, \"end_hour\": 17}"
      , ToolDef "list_connected_accounts" "{\"tool\": \"list_connected_accounts\"}"
      ]
  , skillFetchContext = pure emptySkillContext
  }

insightsSkill :: Skill
insightsSkill = Skill
  { skillName = "insights"
  , skillToolNames = ["search_activities", "get_summary", "get_people_insights"]
  , skillTools =
      [ ToolDef "search_activities" "{\"tool\": \"search_activities\", \"query\": \"search text\", \"limit\": 20}"
      , ToolDef "get_summary" "{\"tool\": \"get_summary\", \"hours\": 24}"
      , ToolDef "get_people_insights" "{\"tool\": \"get_people_insights\", \"search\": \"name\", \"important_only\": true}"
      ]
  , skillFetchContext = pure emptySkillContext
  }

githubSkill :: Skill
githubSkill = Skill
  { skillName = "github"
  , skillToolNames = ["list_repos", "list_commits", "read_file", "view_diff", "list_prs", "view_pr"]
  , skillTools =
      [ ToolDef "list_repos" "{\"tool\": \"list_repos\", \"limit\": 20}"
      , ToolDef "list_commits" "{\"tool\": \"list_commits\", \"repo\": \"owner/repo\", \"branch\": \"main\", \"limit\": 10}"
      , ToolDef "read_file" "{\"tool\": \"read_file\", \"repo\": \"owner/repo\", \"path\": \"src/Main.hs\", \"ref\": \"main\"}"
      , ToolDef "view_diff" "{\"tool\": \"view_diff\", \"repo\": \"owner/repo\", \"commit\": \"abc123\"}"
      , ToolDef "list_prs" "{\"tool\": \"list_prs\", \"repo\": \"owner/repo\", \"state\": \"open\"}"
      , ToolDef "view_pr" "{\"tool\": \"view_pr\", \"repo\": \"owner/repo\", \"number\": 42}"
      ]
  , skillFetchContext = pure emptySkillContext
  }

researchSkill :: Skill
researchSkill = Skill
  { skillName = "research"
  , skillToolNames = ["create_research_plan", "web_search", "write_finding", "complete_research"]
  , skillTools =
      [ ToolDef "create_research_plan" "{\"tool\": \"create_research_plan\", \"topic\": \"Research topic\", \"tasks\": [\"Task 1\", \"Task 2\"]}"
      , ToolDef "web_search" "{\"tool\": \"web_search\", \"query\": \"search query\", \"limit\": 5}"
      , ToolDef "write_finding" "{\"tool\": \"write_finding\", \"session_id\": \"research-20260223-abc123\", \"title\": \"Finding title\", \"content\": \"Finding content\"}"
      , ToolDef "complete_research" "{\"tool\": \"complete_research\", \"session_id\": \"research-20260223-abc123\"}"
      ]
  , skillFetchContext = pure emptySkillContext
  }

-- | Base tools available to all agents
baseTools :: [ToolDef]
baseTools =
  [ ToolDef "search_knowledge" "{\"tool\": \"search_knowledge\", \"query\": \"search text\"}"
  , ToolDef "read_note" "{\"tool\": \"read_note\", \"note_id\": \"id\"}"
  , ToolDef "add_note" "{\"tool\": \"add_note\", \"content\": \"Note content\", \"tags\": [\"tag1\"]}"
  , ToolDef "activate_skill" "{\"tool\": \"activate_skill\", \"skill\": \"scheduler\"}"
  , ToolDef "spawn_agent" "{\"tool\": \"spawn_agent\", \"task\": \"Task description\", \"tools\": [\"web_search\"], \"context\": {}}"
  ]

-- | Result from executing a skill tool
data SkillToolResult
  = SkillToolSuccess Value
  | SkillToolError Text
  deriving (Show, Eq)

-- | Load timezone from IANA name, returning Nothing if invalid
loadTimezone :: Maybe Text -> App (Maybe TZ)
loadTimezone Nothing = pure Nothing
loadTimezone (Just tzName) = do
  result <- liftIO $ try $ loadSystemTZ (T.unpack tzName)
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right tz -> pure (Just tz)

-- | Execute a skill tool by name
-- Routes to the appropriate skill's tool executor
-- timezone: Optional IANA timezone for date/time formatting
executeSkillTool :: Skill -> EntityId -> Text -> Value -> Maybe Text -> App SkillToolResult
executeSkillTool skill accountId toolName params mTimezone
  | skillName skill == "concierge" = executeConcierge accountId toolName params
  | skillName skill == "scheduler" = executeScheduler toolName params mTimezone
  | skillName skill == "insights" = executeInsights toolName params mTimezone
  | skillName skill == "github" = executeGitHub toolName params
  | skillName skill == "research" = executeResearch accountId toolName params
  | otherwise = pure $ SkillToolError $ "Unknown skill: " <> skillName skill

--------------------------------------------------------------------------------
-- Concierge Tool Execution
--------------------------------------------------------------------------------

executeConcierge :: EntityId -> Text -> Value -> App SkillToolResult
executeConcierge _accountId toolName params = case toolName of
  "query_activities" -> do
    case parseActivityFilter params of
      Left err -> pure $ SkillToolError err
      Right filter' -> do
        result <- Concierge.executeToolCall (Concierge.QueryActivities filter')
        toSkillResult result

  "query_people" -> do
    case parsePeopleFilter params of
      Left err -> pure $ SkillToolError err
      Right filter' -> do
        result <- Concierge.executeToolCall (Concierge.QueryPeople filter')
        toSkillResult result

  "update_activities" -> do
    case parseUpdateActivities params of
      Left err -> pure $ SkillToolError err
      Right (ids, updates) -> do
        result <- Concierge.executeToolCall (Concierge.UpdateActivities ids updates)
        toSkillResult result

  _ -> pure $ SkillToolError $ "Unknown concierge tool: " <> toolName

-- Parse activity filter from JSON
parseActivityFilter :: Value -> Either Text Concierge.ActivityFilter
parseActivityFilter v = case fromJSON v of
  Success f -> Right f
  Error e -> Left $ "Invalid activity filter: " <> T.pack e

-- Parse people filter from JSON
parsePeopleFilter :: Value -> Either Text Concierge.PeopleFilter
parsePeopleFilter v = case fromJSON v of
  Success f -> Right f
  Error e -> Left $ "Invalid people filter: " <> T.pack e

-- Parse update activities from JSON
-- Expects: {"activity_ids": [...], "status": "...", "classification": {...}}
parseUpdateActivities :: Value -> Either Text ([Text], Concierge.ActivityUpdates)
parseUpdateActivities (Object v) = do
  case KM.lookup "activity_ids" v of
    Nothing -> Left "Missing activity_ids field"
    Just idsVal -> case fromJSON idsVal of
      Error e -> Left $ "Invalid activity_ids: " <> T.pack e
      Success ids -> case fromJSON (Object v) of
        Error e -> Left $ "Invalid updates: " <> T.pack e
        Success updates -> Right (ids, updates)
parseUpdateActivities _ = Left "Expected object for update_activities"

-- Note: FromJSON instances for Concierge types are defined in Skills.Concierge

toSkillResult :: Concierge.ToolResult -> App SkillToolResult
toSkillResult (Concierge.ToolSuccess v) = pure $ SkillToolSuccess v
toSkillResult (Concierge.ToolError e) = pure $ SkillToolError e

--------------------------------------------------------------------------------
-- Scheduler Tool Execution
--------------------------------------------------------------------------------

executeScheduler :: Text -> Value -> Maybe Text -> App SkillToolResult
executeScheduler toolName params mTimezone = do
  -- Convert timezone string to TZ
  mTz <- loadTimezone mTimezone
  case toolName of
    "query_calendar" -> do
      case parseCalendarQuery params of
        Left err -> pure $ SkillToolError err
        Right query -> do
          result <- Scheduler.executeToolCall mTz (Scheduler.QueryCalendar query)
          toSchedulerResult result

    "find_free_slots" -> do
      case parseFreeSlotQuery params of
        Left err -> pure $ SkillToolError err
        Right query -> do
          result <- Scheduler.executeToolCall mTz (Scheduler.FindFreeSlots query)
          toSchedulerResult result

    "list_connected_accounts" -> do
      result <- Scheduler.executeToolCall mTz Scheduler.ListConnectedAccounts
      toSchedulerResult result

    _ -> pure $ SkillToolError $ "Unknown scheduler tool: " <> toolName

parseCalendarQuery :: Value -> Either Text Scheduler.CalendarQuery
parseCalendarQuery v = case fromJSON v of
  Success q -> Right q
  Error e -> Left $ "Invalid calendar query: " <> T.pack e

parseFreeSlotQuery :: Value -> Either Text Scheduler.FreeSlotQuery
parseFreeSlotQuery v = case fromJSON v of
  Success q -> Right q
  Error e -> Left $ "Invalid free slot query: " <> T.pack e

-- Note: FromJSON instances for Scheduler queries are defined in Skills.Scheduler

toSchedulerResult :: Scheduler.ToolResult -> App SkillToolResult
toSchedulerResult (Scheduler.ToolSuccess v) = pure $ SkillToolSuccess v
toSchedulerResult (Scheduler.ToolError e) = pure $ SkillToolError e

--------------------------------------------------------------------------------
-- Insights Tool Execution
--------------------------------------------------------------------------------

executeInsights :: Text -> Value -> Maybe Text -> App SkillToolResult
executeInsights toolName params mTimezone = do
  -- Convert timezone string to TZ
  mTz <- loadTimezone mTimezone
  case toolName of
    "search_activities" -> do
      case parseSearchQuery params of
        Left err -> pure $ SkillToolError err
        Right query -> do
          result <- Insights.executeToolCall mTz (Insights.SearchActivities query)
          toInsightsResult result

    "get_summary" -> do
      case parseSummaryQuery params of
        Left err -> pure $ SkillToolError err
        Right query -> do
          result <- Insights.executeToolCall mTz (Insights.GetSummary query)
          toInsightsResult result

    "get_people_insights" -> do
      case parsePeopleInsightsQuery params of
        Left err -> pure $ SkillToolError err
        Right query -> do
          result <- Insights.executeToolCall mTz (Insights.GetPeopleInsights query)
          toInsightsResult result

    _ -> pure $ SkillToolError $ "Unknown insights tool: " <> toolName

parseSearchQuery :: Value -> Either Text Insights.SearchQuery
parseSearchQuery v = case fromJSON v of
  Success q -> Right q
  Error e -> Left $ "Invalid search query: " <> T.pack e

parseSummaryQuery :: Value -> Either Text Insights.SummaryQuery
parseSummaryQuery v = case fromJSON v of
  Success q -> Right q
  Error e -> Left $ "Invalid summary query: " <> T.pack e

parsePeopleInsightsQuery :: Value -> Either Text Insights.PeopleQuery
parsePeopleInsightsQuery v = case fromJSON v of
  Success q -> Right q
  Error e -> Left $ "Invalid people insights query: " <> T.pack e

-- Note: FromJSON instances for Insights queries are defined in Skills.Insights

toInsightsResult :: Insights.ToolResult -> App SkillToolResult
toInsightsResult (Insights.ToolSuccess v) = pure $ SkillToolSuccess v
toInsightsResult (Insights.ToolError e) = pure $ SkillToolError e

--------------------------------------------------------------------------------
-- GitHub Tool Execution
--------------------------------------------------------------------------------

executeGitHub :: Text -> Value -> App SkillToolResult
executeGitHub toolName params = do
  case parseGitHubToolCall toolName params of
    Left err -> pure $ SkillToolError err
    Right toolCall -> do
      result <- GitHub.executeToolCall toolCall
      toGitHubResult result

parseGitHubToolCall :: Text -> Value -> Either Text GitHub.GitHubToolCall
parseGitHubToolCall toolName params =
  -- Add the tool name back to params for parsing
  let paramsWithTool = case params of
        Object obj -> Object $ KM.insert "tool" (toJSON toolName) obj
        _ -> params
  in case fromJSON paramsWithTool of
    Success call -> Right call
    Error e -> Left $ "Invalid " <> toolName <> " params: " <> T.pack e

toGitHubResult :: GitHub.ToolResult -> App SkillToolResult
toGitHubResult (GitHub.ToolSuccess v) = pure $ SkillToolSuccess v
toGitHubResult (GitHub.ToolError e) = pure $ SkillToolError e

--------------------------------------------------------------------------------
-- Research Tool Execution
--------------------------------------------------------------------------------

executeResearch :: EntityId -> Text -> Value -> App SkillToolResult
executeResearch accountId toolName params = do
  case parseResearchToolCall toolName params of
    Left err -> pure $ SkillToolError err
    Right toolCall -> do
      result <- Research.executeToolCall accountId toolCall
      toResearchResult result

parseResearchToolCall :: Text -> Value -> Either Text Research.ResearchToolCall
parseResearchToolCall toolName params =
  -- Add the tool name back to params for parsing
  let paramsWithTool = case params of
        Object obj -> Object $ KM.insert "tool" (toJSON toolName) obj
        _ -> params
  in case fromJSON paramsWithTool of
    Success call -> Right call
    Error e -> Left $ "Invalid " <> toolName <> " params: " <> T.pack e

toResearchResult :: Research.ToolResult -> App SkillToolResult
toResearchResult (Research.ToolSuccess v) = pure $ SkillToolSuccess v
toResearchResult (Research.ToolError e) = pure $ SkillToolError e
