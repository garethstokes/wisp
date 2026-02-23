module Skills.Concierge.Classifier
  ( classifyActivity
  , classifyActivityWithProjects
  , buildClassificationPrompt
  , buildClassificationPromptWithProjects
  , parseClassificationResponse
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import qualified Data.Aeson
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..), ActivitySource(..))
import Domain.Classification (Classification)
import Domain.Document (Document(..))
import Infra.Claude.Client (callClaude)
import Infra.Db.Document (getAllProjects)

-- Classify an activity using Claude (simple version without projects)
classifyActivity :: Activity -> App (Either Text Classification)
classifyActivity activity = do
  cfg <- getConfig
  let apiKey = cfg.claude.apiKey
  let model = cfg.claude.model
  let source = activitySourceText (activitySource activity)
  let prompt = buildClassificationPrompt source (activityTitle activity) (activityRaw activity)
  result <- liftIO $ callClaude apiKey model prompt
  pure $ case result of
    Left err -> Left err
    Right respText -> parseClassificationResponse respText

-- Classify an activity with project context
-- Fetches known projects and includes them in the prompt for assignment
classifyActivityWithProjects :: Activity -> App (Either Text Classification)
classifyActivityWithProjects activity = do
  -- Fetch known projects
  projects <- getAllProjects
  let projectTags = extractProjectTags projects

  cfg <- getConfig
  let apiKey = cfg.claude.apiKey
  let model = cfg.claude.model
  let source = activitySourceText (activitySource activity)
  let prompt = buildClassificationPromptWithProjects source (activityTitle activity) (activityRaw activity) projectTags
  result <- liftIO $ callClaude apiKey model prompt
  pure $ case result of
    Left err -> Left err
    Right respText -> parseClassificationResponse respText

-- Extract project tags from documents
-- Uses the first tag from each project document
extractProjectTags :: [Document] -> [Text]
extractProjectTags = mapMaybe getFirstTag
  where
    getFirstTag doc = case documentTags doc of
      (tag:_) -> Just tag
      [] -> Nothing

-- Convert ActivitySource to text for prompts
activitySourceText :: ActivitySource -> Text
activitySourceText Email = "email"
activitySourceText Calendar = "calendar"
activitySourceText Conversation = "conversation"
activitySourceText Note = "note"
activitySourceText GitHubEvent = "github_event"
activitySourceText UnknownSource = "unknown"

-- Build the classification prompt
buildClassificationPrompt :: Text -> Maybe Text -> Value -> Text
buildClassificationPrompt source mTitle raw = T.unlines
  [ "You are classifying an incoming " <> source <> " for a personal assistant."
  , ""
  , "Analyze the following and respond with ONLY a JSON object (no markdown, no explanation):"
  , ""
  , "Title: " <> maybe "(none)" id mTitle
  , ""
  , "Raw data:"
  , TL.toStrict $ TLE.decodeUtf8 $ encode raw
  , ""
  , "Respond with this exact JSON structure:"
  , "{"
  , "  \"personas\": [\"work\"|\"home\"|\"personal\"],  // which life areas this relates to"
  , "  \"activity_type\": \"request\"|\"information\"|\"action_required\"|\"fyi\"|\"event\","
  , "  \"urgency\": \"high\"|\"normal\"|\"low\","
  , "  \"autonomy_tier\": 1-4,  // 1=ignore, 2=note, 3=draft response, 4=needs attention"
  , "  \"confidence\": 0.0-1.0,  // how confident you are in this classification"
  , "  \"summary\": \"Brief 1-sentence summary\","
  , "  \"reasoning\": \"Why you classified it this way\","
  , "  \"suggested_actions\": [\"action1\", \"action2\"],  // what could be done"
  , "  \"option_framing\": \"How to present to user if surfaced\"  // null for tier 1-2"
  , "}"
  , ""
  , "Guidelines for autonomy_tier:"
  , "- Tier 1: Automated notifications, newsletters, receipts - can be silently processed"
  , "- Tier 2: FYI items, updates, non-urgent info - note but don't surface"
  , "- Tier 3: Requests needing response, calendar invites - draft action for review"
  , "- Tier 4: Urgent items, important people, time-sensitive - surface immediately"
  , ""
  , "For suggested_actions, list 1-3 concrete actions the user could take."
  , "For option_framing, write how you would present this to the user as an option (not a demand)."
  , "Only include option_framing for tier 3-4 items; set to null for tier 1-2."
  ]

-- Build classification prompt with known projects for project assignment
buildClassificationPromptWithProjects :: Text -> Maybe Text -> Value -> [Text] -> Text
buildClassificationPromptWithProjects source mTitle raw projects = T.unlines $
  [ "You are classifying an incoming " <> source <> " for a personal assistant."
  , ""
  , "Analyze the following and respond with ONLY a JSON object (no markdown, no explanation):"
  , ""
  , "Title: " <> maybe "(none)" id mTitle
  , ""
  , "Raw data:"
  , TL.toStrict $ TLE.decodeUtf8 $ encode raw
  , ""
  ] ++ projectsSection projects ++
  [ "Respond with this exact JSON structure:"
  , "{"
  , "  \"personas\": [\"work\"|\"home\"|\"personal\"],  // which life areas this relates to"
  , "  \"activity_type\": \"request\"|\"information\"|\"action_required\"|\"fyi\"|\"event\","
  , "  \"urgency\": \"high\"|\"normal\"|\"low\","
  , "  \"autonomy_tier\": 1-4,  // 1=ignore, 2=note, 3=draft response, 4=needs attention"
  , "  \"confidence\": 0.0-1.0,  // how confident you are in this classification"
  , "  \"summary\": \"Brief 1-sentence summary\","
  , "  \"reasoning\": \"Why you classified it this way\","
  , "  \"suggested_actions\": [\"action1\", \"action2\"],  // what could be done"
  , "  \"option_framing\": \"How to present to user if surfaced\",  // null for tier 1-2"
  , "  \"projects\": [{\"name\": \"project-tag\", \"confidence\": 0.0-1.0}]  // matched projects"
  , "}"
  , ""
  , "Guidelines for autonomy_tier:"
  , "- Tier 1: Automated notifications, newsletters, receipts - can be silently processed"
  , "- Tier 2: FYI items, updates, non-urgent info - note but don't surface"
  , "- Tier 3: Requests needing response, calendar invites - draft action for review"
  , "- Tier 4: Urgent items, important people, time-sensitive - surface immediately"
  , ""
  , "For suggested_actions, list 1-3 concrete actions the user could take."
  , "For option_framing, write how you would present this to the user as an option (not a demand)."
  , "Only include option_framing for tier 3-4 items; set to null for tier 1-2."
  ] ++ projectGuidelines projects
  where
    projectsSection :: [Text] -> [Text]
    projectsSection [] = []
    projectsSection ps =
      [ "## Known Projects"
      , ""
      ] ++ map (\p -> "- " <> p) ps ++
      [ ""
      ]

    projectGuidelines :: [Text] -> [Text]
    projectGuidelines [] = []
    projectGuidelines _ =
      [ ""
      , "## Project Assignment Guidelines"
      , "- Match activities to known projects based on content, participants, or context"
      , "- Include confidence score (0.0-1.0) for each project assignment"
      , "- An activity can belong to multiple projects"
      , "- Only assign to projects you're confident about (>0.5 confidence)"
      , "- If no projects match, return an empty projects array"
      ]

-- Parse classification response from Claude
parseClassificationResponse :: Text -> Either Text Classification
parseClassificationResponse txt =
  case Data.Aeson.decode (TLE.encodeUtf8 $ TL.fromStrict txt) of
    Just c -> Right c
    Nothing -> Left $ "Failed to parse classification: " <> T.take 200 txt
