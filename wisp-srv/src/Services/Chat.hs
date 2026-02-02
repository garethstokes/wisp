module Services.Chat
  ( assembleContext
  , buildSystemPrompt
  , processChat
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Chat (ChatContext(..))
import Infra.Db.Activity (getRecentActivities, getTodaysCalendarEvents, getActivitiesByStatus, getPendingEmails, insertConversation)
import Infra.Claude.Client (callClaudeWithSystem)
import App.Monad (App, Env(..))
import App.Config (Config(..), ClaudeConfig(..))

-- Assemble context for the LLM
assembleContext :: Text -> App ChatContext
assembleContext _query = do
  calendar <- getTodaysCalendarEvents
  recent <- getRecentActivities 24
  pendingEmails <- getPendingEmails 50
  quarantined <- getActivitiesByStatus Quarantined 100
  surfaced <- getActivitiesByStatus Surfaced 100
  pure ChatContext
    { contextCalendarEvents = calendar
    , contextRecentActivities = recent
    , contextPendingEmails = pendingEmails
    , contextQuarantineCount = length quarantined
    , contextSurfacedCount = length surfaced
    , contextMentionedPeople = []
    }

-- Build the system prompt with context
buildSystemPrompt :: ChatContext -> Text
buildSystemPrompt ctx = T.unlines
  [ "You are Wisp, a personal assistant. You help by providing information"
  , "and options, never pressure or demands."
  , ""
  , "Rules:"
  , "- Never say \"you should\" or \"don't forget\" - offer observations instead"
  , "- Frame gaps in schedule as possibilities, not obligations"
  , "- When discussing progress, celebrate what happened, don't mention what didn't"
  , "- Be concise and conversational"
  , ""
  , "## Today's Calendar"
  , formatCalendar (contextCalendarEvents ctx)
  , ""
  , "## Pending Emails (" <> T.pack (show (length (contextPendingEmails ctx))) <> " total)"
  , formatEmails (contextPendingEmails ctx)
  , ""
  , "## Context"
  , "- " <> T.pack (show (length (contextRecentActivities ctx))) <> " activities in last 24 hours"
  , "- " <> T.pack (show (contextQuarantineCount ctx)) <> " items in quarantine needing review"
  , "- " <> T.pack (show (contextSurfacedCount ctx)) <> " items surfaced for attention"
  ]

formatCalendar :: [Activity] -> Text
formatCalendar [] = "No events scheduled today."
formatCalendar events = T.unlines $ map formatEvent events
  where
    formatEvent a = "- " <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\t -> " at " <> T.pack (show t)) (activityStartsAt a)

formatEmails :: [Activity] -> Text
formatEmails [] = "No pending emails."
formatEmails emails = T.unlines $ map formatEmail (take 20 emails)  -- Show top 20
  where
    formatEmail a = "- " <> fromMaybe "(no subject)" (activityTitle a)
      <> maybe "" (\s -> " from " <> s) (activitySenderEmail a)

-- Process a chat message
processChat :: Text -> App (Either Text Text)
processChat query = do
  ctx <- assembleContext query
  let systemPrompt = buildSystemPrompt ctx
  claudeCfg <- asks (claude . config)
  result <- liftIO $ callClaudeWithSystem
    (apiKey claudeCfg)
    (model claudeCfg)
    systemPrompt
    query
  -- Log the conversation
  case result of
    Right response -> do
      _ <- insertConversation query response
      pure $ Right response
    Left err -> pure $ Left err
