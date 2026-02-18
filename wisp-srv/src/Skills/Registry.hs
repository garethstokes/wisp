-- | Skill Registry
-- Maps skill names to their implementations.
-- Each skill provides tools and can fetch its own context.
module Skills.Registry
  ( Skill(..)
  , SkillContext(..)
  , SkillToolResult(..)
  , getSkill
  , allSkillNames
  , emptySkillContext
  , executeSkillTool
  ) where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import Domain.Activity (Activity)
import Domain.Person (Person)
import Domain.Id (EntityId)
import App.Monad (App)
import qualified Skills.Concierge as Concierge
import qualified Skills.Scheduler as Scheduler
import qualified Skills.Insights as Insights

-- | Context that a skill can provide to the agent
data SkillContext = SkillContext
  { skillContextActivities :: [Activity]  -- ^ Relevant activities for this skill
  , skillContextPeople     :: [Person]    -- ^ Relevant people for this skill
  , skillContextExtra      :: Value       -- ^ Skill-specific additional data
  } deriving (Show)

-- | Empty skill context
emptySkillContext :: SkillContext
emptySkillContext = SkillContext [] [] (toJSON ())

-- | A skill provides tools and can fetch its own context
data Skill = Skill
  { skillName         :: Text              -- ^ Skill identifier (e.g. "concierge")
  , skillToolNames    :: [Text]            -- ^ Tool names this skill provides
  , skillFetchContext :: App SkillContext  -- ^ How to get skill-specific context
  }

-- | Registry of available skill names
allSkillNames :: [Text]
allSkillNames = ["concierge", "scheduler", "insights"]

-- | Look up a skill by name
getSkill :: Text -> Maybe Skill
getSkill "concierge" = Just conciergeSkill
getSkill "scheduler" = Just schedulerSkill
getSkill "insights"  = Just insightsSkill
getSkill _           = Nothing

-- Skill definitions
-- Context fetching will be implemented when we wire up the full agent core

conciergeSkill :: Skill
conciergeSkill = Skill
  { skillName = "concierge"
  , skillToolNames = ["update_activities", "query_activities", "query_people"]
  , skillFetchContext = pure emptySkillContext  -- TODO: fetch pending activities
  }

schedulerSkill :: Skill
schedulerSkill = Skill
  { skillName = "scheduler"
  , skillToolNames = ["query_calendar", "find_free_slots"]
  , skillFetchContext = pure emptySkillContext  -- TODO: fetch calendar events
  }

insightsSkill :: Skill
insightsSkill = Skill
  { skillName = "insights"
  , skillToolNames = ["search_activities", "get_summary", "get_people_insights"]
  , skillFetchContext = pure emptySkillContext  -- TODO: fetch recent activity stats
  }

-- | Result from executing a skill tool
data SkillToolResult
  = SkillToolSuccess Value
  | SkillToolError Text
  deriving (Show, Eq)

-- | Execute a skill tool by name
-- Routes to the appropriate skill's tool executor
executeSkillTool :: Skill -> EntityId -> Text -> Value -> App SkillToolResult
executeSkillTool skill _accountId toolName params
  | skillName skill == "concierge" = executeConcierge toolName params
  | skillName skill == "scheduler" = executeScheduler toolName params
  | skillName skill == "insights" = executeInsights toolName params
  | otherwise = pure $ SkillToolError $ "Unknown skill: " <> skillName skill

-- | Execute concierge tool
executeConcierge :: Text -> Value -> App SkillToolResult
executeConcierge toolName params = do
  let mToolCall = parseConciergeToolCall toolName params
  case mToolCall of
    Nothing -> pure $ SkillToolError $ "Unknown concierge tool: " <> toolName
    Just toolCall -> do
      result <- Concierge.executeToolCall toolCall
      pure $ case result of
        Concierge.ToolSuccess v -> SkillToolSuccess v
        Concierge.ToolError e -> SkillToolError e

-- | Parse concierge tool call from name and params
parseConciergeToolCall :: Text -> Value -> Maybe Concierge.ConciergeToolCall
parseConciergeToolCall "update_activities" params = parseUpdateActivities params
parseConciergeToolCall "query_activities" params = parseQueryActivities params
parseConciergeToolCall "query_people" params = parseQueryPeople params
parseConciergeToolCall _ _ = Nothing

parseUpdateActivities :: Value -> Maybe Concierge.ConciergeToolCall
parseUpdateActivities _ = Nothing  -- TODO: parse from Value

parseQueryActivities :: Value -> Maybe Concierge.ConciergeToolCall
parseQueryActivities _ = Just $ Concierge.QueryActivities Concierge.ActivityFilter
  { Concierge.filterStatus = Nothing
  , Concierge.filterLimit = Just 20
  , Concierge.filterSince = Nothing
  , Concierge.filterBefore = Nothing
  }

parseQueryPeople :: Value -> Maybe Concierge.ConciergeToolCall
parseQueryPeople _ = Just $ Concierge.QueryPeople Concierge.PeopleFilter
  { Concierge.peopleEmail = Nothing
  , Concierge.peopleSearch = Nothing
  , Concierge.peopleLimit = Just 20
  }

-- | Execute scheduler tool
executeScheduler :: Text -> Value -> App SkillToolResult
executeScheduler toolName params = do
  let mToolCall = parseSchedulerToolCall toolName params
  case mToolCall of
    Nothing -> pure $ SkillToolError $ "Unknown scheduler tool: " <> toolName
    Just toolCall -> do
      result <- Scheduler.executeToolCall Nothing toolCall  -- No timezone in this context
      pure $ case result of
        Scheduler.ToolSuccess v -> SkillToolSuccess v
        Scheduler.ToolError e -> SkillToolError e

parseSchedulerToolCall :: Text -> Value -> Maybe Scheduler.SchedulerToolCall
parseSchedulerToolCall "query_calendar" _ = Just $ Scheduler.QueryCalendar Scheduler.CalendarQuery
  { Scheduler.calendarDays = Just 7
  , Scheduler.calendarDate = Nothing
  }
parseSchedulerToolCall "find_free_slots" _ = Just $ Scheduler.FindFreeSlots Scheduler.FreeSlotQuery
  { Scheduler.slotDays = Just 7
  , Scheduler.slotDuration = Just 60
  , Scheduler.slotStartHour = Just 9
  , Scheduler.slotEndHour = Just 17
  }
parseSchedulerToolCall _ _ = Nothing

-- | Execute insights tool
executeInsights :: Text -> Value -> App SkillToolResult
executeInsights toolName params = do
  let mToolCall = parseInsightsToolCall toolName params
  case mToolCall of
    Nothing -> pure $ SkillToolError $ "Unknown insights tool: " <> toolName
    Just toolCall -> do
      result <- Insights.executeToolCall Nothing toolCall  -- No timezone in this context
      pure $ case result of
        Insights.ToolSuccess v -> SkillToolSuccess v
        Insights.ToolError e -> SkillToolError e

parseInsightsToolCall :: Text -> Value -> Maybe Insights.InsightsToolCall
parseInsightsToolCall "search_activities" _ = Just $ Insights.SearchActivities Insights.SearchQuery
  { Insights.searchTerm = ""
  , Insights.searchLimit = Just 20
  }
parseInsightsToolCall "get_summary" _ = Just $ Insights.GetSummary Insights.SummaryQuery
  { Insights.summaryHours = Just 24
  }
parseInsightsToolCall "get_people_insights" _ = Just $ Insights.GetPeopleInsights Insights.PeopleQuery
  { Insights.peopleSearch = Nothing
  , Insights.peopleImportantOnly = Nothing
  }
parseInsightsToolCall _ _ = Nothing
