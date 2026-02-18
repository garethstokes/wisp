-- | Skill Registry
-- Maps skill names to their implementations.
-- Each skill provides tools and can fetch its own context.
module Skills.Registry
  ( Skill(..)
  , SkillContext(..)
  , getSkill
  , allSkillNames
  , emptySkillContext
  ) where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import Domain.Activity (Activity)
import Domain.Person (Person)
import App.Monad (App)

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
