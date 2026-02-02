module Services.Chat
  ( assembleContext
  , buildSystemPrompt
  , processChat
  , executeAction
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Chat (ChatContext(..))
import Domain.ChatAction (ChatAction(..), ActionFilter(..), ActionResult(..))
import Domain.Id (EntityId(..), unEntityId)
import Infra.Db.Activity (getRecentActivities, getTodaysCalendarEvents, getActivitiesByStatus, getPendingEmails, insertConversation, updateActivityStatus, getActivity)
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
  needsReview <- getActivitiesByStatus NeedsReview 100
  pure ChatContext
    { contextCalendarEvents = calendar
    , contextRecentActivities = recent
    , contextPendingEmails = pendingEmails
    , contextQuarantined = quarantined
    , contextSurfaced = surfaced
    , contextNeedsReview = needsReview
    , contextMentionedPeople = []
    }

-- Build the system prompt with context
buildSystemPrompt :: ChatContext -> Text
buildSystemPrompt ctx = T.unlines
  [ "You are Wisp, a personal assistant that can take actions on activities."
  , ""
  , "## Response Format"
  , "You MUST respond with valid JSON in this exact format:"
  , "```json"
  , "{"
  , "  \"action\": \"<action_type>\","
  , "  \"response\": \"<message to user>\","
  , "  ... action-specific fields ..."
  , "}"
  , "```"
  , ""
  , "## Available Actions"
  , ""
  , "### mark_complete - Mark activities as processed"
  , "Use when user wants to complete, finish, or mark items as done."
  , "{\"action\": \"mark_complete\", \"filter\": \"all_surfaced\", \"response\": \"Marked N items complete\"}"
  , "Filters: all_surfaced, all_quarantined, all_needs_review, by_ids"
  , "For by_ids: {\"action\": \"mark_complete\", \"filter\": \"by_ids\", \"ids\": [\"id1\", \"id2\"], \"response\": \"...\"}"
  , ""
  , "### approve - Approve a quarantined item (moves to surfaced)"
  , "{\"action\": \"approve\", \"id\": \"<activity_id>\", \"response\": \"Approved: ...\"}"
  , ""
  , "### dismiss - Archive/dismiss an activity"
  , "{\"action\": \"dismiss\", \"id\": \"<activity_id>\", \"response\": \"Dismissed: ...\"}"
  , ""
  , "### none - Just respond, no action needed"
  , "{\"action\": \"none\", \"response\": \"Your conversational response here\"}"
  , ""
  , "## Style Rules"
  , "- Never say \"you should\" or \"don't forget\""
  , "- Be concise and conversational"
  , ""
  , "## Current State"
  , ""
  , "### Surfaced (" <> T.pack (show (length (contextSurfaced ctx))) <> " items needing attention)"
  , formatActivitiesWithIds (contextSurfaced ctx)
  , ""
  , "### Quarantined (" <> T.pack (show (length (contextQuarantined ctx))) <> " items pending approval)"
  , formatActivitiesWithIds (contextQuarantined ctx)
  , ""
  , "### Needs Review (" <> T.pack (show (length (contextNeedsReview ctx))) <> " items)"
  , formatActivitiesWithIds (contextNeedsReview ctx)
  , ""
  , "### Today's Calendar"
  , formatCalendar (contextCalendarEvents ctx)
  ]

formatActivitiesWithIds :: [Activity] -> Text
formatActivitiesWithIds [] = "(none)"
formatActivitiesWithIds activities = T.unlines $ map formatOne (take 50 activities)
  where
    formatOne a = "- [" <> unEntityId (activityId a) <> "] "
      <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\s -> " (from: " <> s <> ")") (activitySenderEmail a)

formatCalendar :: [Activity] -> Text
formatCalendar [] = "No events scheduled today."
formatCalendar events = T.unlines $ map formatEvent events
  where
    formatEvent a = "- [" <> unEntityId (activityId a) <> "] "
      <> fromMaybe "(no title)" (activityTitle a)
      <> maybe "" (\t -> " at " <> T.pack (show t)) (activityStartsAt a)

-- Execute an action and return the result
executeAction :: ChatAction -> App ActionResult
executeAction (NoAction msg) = pure $ ActionSuccess msg 0
executeAction (MarkComplete filterType) = do
  activities <- case filterType of
    AllSurfaced -> getActivitiesByStatus Surfaced 1000
    AllQuarantined -> getActivitiesByStatus Quarantined 1000
    AllNeedsReview -> getActivitiesByStatus NeedsReview 1000
    ByIds ids -> do
      results <- mapM (getActivity . EntityId) ids
      pure $ mapMaybe id results
  -- Mark all as Processed
  mapM_ (\a -> updateActivityStatus (activityId a) Processed) activities
  pure $ ActionSuccess ("Marked " <> T.pack (show (length activities)) <> " activities as complete") (length activities)
executeAction (ApproveActivity aid) = do
  mActivity <- getActivity (EntityId aid)
  case mActivity of
    Nothing -> pure $ ActionError ("Activity not found: " <> aid)
    Just activity -> do
      updateActivityStatus (activityId activity) Surfaced
      pure $ ActionSuccess ("Approved: " <> fromMaybe "(no title)" (activityTitle activity)) 1
executeAction (DismissActivity aid) = do
  mActivity <- getActivity (EntityId aid)
  case mActivity of
    Nothing -> pure $ ActionError ("Activity not found: " <> aid)
    Just activity -> do
      updateActivityStatus (activityId activity) Archived
      pure $ ActionSuccess ("Dismissed: " <> fromMaybe "(no title)" (activityTitle activity)) 1

-- Parse JSON response from Claude, handling markdown code blocks
parseActionResponse :: Text -> Either Text ChatAction
parseActionResponse raw =
  let -- Strip markdown code blocks if present
      stripped = stripCodeBlock raw
      jsonBytes = BL.fromStrict (encodeUtf8 stripped)
  in case decode jsonBytes of
    Just action -> Right action
    Nothing -> Left $ "Failed to parse action from response: " <> raw

-- Strip ```json ... ``` wrapper if present
stripCodeBlock :: Text -> Text
stripCodeBlock t =
  let lines' = T.lines t
      -- Remove leading ```json or ```
      withoutStart = case lines' of
        (l:rest) | "```" `T.isPrefixOf` l -> rest
        other -> other
      -- Remove trailing ```
      withoutEnd = case reverse withoutStart of
        (l:rest) | l == "```" -> reverse rest
        other -> reverse other
  in T.unlines withoutEnd

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
  case result of
    Left err -> pure $ Left err
    Right response -> do
      -- Log the raw conversation
      _ <- insertConversation query response
      -- Parse and execute the action
      case parseActionResponse response of
        Left err -> pure $ Left err
        Right action -> do
          actionResult <- executeAction action
          case actionResult of
            ActionSuccess msg _ -> pure $ Right msg
            ActionError err -> pure $ Left err
