module Services.Classifier
  ( classifyActivity
  , buildClassificationPrompt
  , parseClassificationResponse
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import qualified Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))
import Domain.Activity (Activity(..), ActivitySource(..))
import Domain.Classification (Classification)
import Infra.Claude.Client (callClaude)

-- Classify an activity using Claude
classifyActivity :: Activity -> App (Either Text Classification)
classifyActivity activity = do
  cfg <- getConfig
  let apiKey = cfg.claude.apiKey
  let model = cfg.claude.model
  let source = case activitySource activity of
        Email -> "email"
        Calendar -> "calendar"
        Conversation -> "conversation"
  let prompt = buildClassificationPrompt source (activityTitle activity) (activityRaw activity)
  result <- liftIO $ callClaude apiKey model prompt
  pure $ case result of
    Left err -> Left err
    Right respText -> parseClassificationResponse respText

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
  , "  \"summary\": \"Brief 1-sentence summary\""
  , "}"
  , ""
  , "Guidelines for autonomy_tier:"
  , "- Tier 1: Automated notifications, newsletters, receipts - can be silently processed"
  , "- Tier 2: FYI items, updates, non-urgent info - note but don't surface"
  , "- Tier 3: Requests needing response, calendar invites - draft action for review"
  , "- Tier 4: Urgent items, important people, time-sensitive - surface immediately"
  ]

-- Parse classification response from Claude
parseClassificationResponse :: Text -> Either Text Classification
parseClassificationResponse txt =
  case Data.Aeson.decode (TLE.encodeUtf8 $ TL.fromStrict txt) of
    Just c -> Right c
    Nothing -> Left $ "Failed to parse classification: " <> T.take 200 txt
