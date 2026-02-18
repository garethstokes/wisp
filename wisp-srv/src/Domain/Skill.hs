-- | Domain model for Skills.
-- Skills are capability modules that agents can activate.
-- A skill provides:
--   - A prompt (stored in knowledge as a Note with tags [skill:NAME, prompt])
--   - A set of tools (implemented in Skills.* modules)
--   - Context fetching logic (how to gather relevant data)
module Domain.Skill
  ( SkillName
  , skillTag
  , skillPromptTags
  , parseSkillTag
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A skill name, e.g. "concierge", "scheduler"
type SkillName = Text

-- | Create the tag for a skill's prompt note
-- e.g. skillTag "concierge" = "skill:concierge"
skillTag :: SkillName -> Text
skillTag name = "skill:" <> name

-- | Tags that identify a skill's prompt note
-- e.g. skillPromptTags "concierge" = ["skill:concierge", "prompt"]
skillPromptTags :: SkillName -> [Text]
skillPromptTags name = [skillTag name, "prompt"]

-- | Parse skill name from a tag like "skill:concierge"
parseSkillTag :: Text -> Maybe SkillName
parseSkillTag t = case T.stripPrefix "skill:" t of
  Just name | not (T.null name) -> Just name
  _ -> Nothing
