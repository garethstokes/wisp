module Services.Chat
  ( assembleContext
  , buildPrompt
  , processChat
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Chat (ChatContext(..))
import Infra.Db.Activity (getRecentActivities, getTodaysCalendarEvents, getActivitiesByStatus)
import App.Monad (App)

-- Assemble context for the LLM
assembleContext :: Text -> App ChatContext
assembleContext _query = do
  calendar <- getTodaysCalendarEvents
  recent <- getRecentActivities 24
  quarantined <- getActivitiesByStatus Quarantined 100
  surfaced <- getActivitiesByStatus Surfaced 100
  pure ChatContext
    { contextCalendarEvents = calendar
    , contextRecentActivities = recent
    , contextQuarantineCount = length quarantined
    , contextSurfacedCount = length surfaced
    , contextMentionedPeople = []  -- TODO: extract mentions from query
    }

-- Build the prompt for Claude
buildPrompt :: ChatContext -> Text -> Text
buildPrompt ctx query = T.unlines
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
  , "## Recent Activity Summary"
  , "- " <> T.pack (show (length (contextRecentActivities ctx))) <> " activities in last 24 hours"
  , "- " <> T.pack (show (contextQuarantineCount ctx)) <> " items in quarantine"
  , "- " <> T.pack (show (contextSurfacedCount ctx)) <> " items surfaced for attention"
  , ""
  , "## User Question"
  , query
  ]

formatCalendar :: [Activity] -> Text
formatCalendar [] = "No events scheduled today."
formatCalendar events = T.unlines $ map formatEvent events
  where
    formatEvent a = "- " <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\t -> " at " <> T.pack (show t)) (activityStartsAt a)

-- Process a chat message (context assembly + LLM call placeholder)
processChat :: Text -> App Text
processChat query = do
  ctx <- assembleContext query
  let prompt = buildPrompt ctx query
  -- For now, return the prompt as a placeholder
  -- Will be replaced with actual LLM call in next task
  pure $ "I received your question: " <> query <> "\n\n[Debug: prompt length = "
    <> T.pack (show (T.length prompt)) <> " chars]"
