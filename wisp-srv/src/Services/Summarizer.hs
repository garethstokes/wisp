-- | Session Summarizer Service
-- Generates summaries of ended sessions and stores them as documents.
module Services.Summarizer
  ( summarizeSession
  , summarizeUnsummarizedSessions
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Session (Session(..))
import Domain.Document (NewDocument(..), DocumentType(..), NoteData(..))
import Domain.Chat (ChatMessage(..))
import Infra.Db.Session (getUnsummarizedSessions, markSummarized)
import Infra.Db.Document (insertDocument)
import Infra.Claude.Client (callClaude)
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))

-- | Summarize a session and store as a document
summarizeSession :: Session -> App ()
summarizeSession session = do
  -- Build summary prompt
  let messagesText = T.unlines $ map formatMessage (sessionMessages session)
      prompt = T.unlines
        [ "Summarize this conversation in 2-3 sentences. Focus on:"
        , "- Key decisions made"
        , "- Information learned"
        , "- Tasks discussed"
        , ""
        , "Conversation:"
        , messagesText
        ]

  -- Call LLM for summary
  cfg <- getConfig
  result <- liftIO $ callClaude cfg.claude.apiKey cfg.claude.model prompt

  case result of
    Left _err -> pure ()  -- Skip on error, will retry later
    Right summaryText -> do
      -- Store as document
      let noteData = NoteData
            { noteTitle = "Session summary: " <> sessionAgentId session
            , noteContent = Just summaryText
            }
          tags = ["session-summary", "agent:" <> sessionAgentId session]
      _ <- insertDocument NewDocument
        { newDocTenantId = Nothing  -- TODO: get tenant from session
        , newDocType = NoteDoc
        , newDocData = toJSON noteData
        , newDocTags = tags
        , newDocConfidence = Just 0.8
        , newDocSource = Just "summarizer"
        , newDocSupersedesId = Nothing
        }
      -- Mark session as summarized
      markSummarized (sessionId session)
  where
    formatMessage :: ChatMessage -> Text
    formatMessage msg = messageRole msg <> ": " <> messageContent msg

-- | Summarize all unsummarized sessions for an agent
summarizeUnsummarizedSessions :: Text -> App ()
summarizeUnsummarizedSessions agentId = do
  sessions <- getUnsummarizedSessions agentId
  forM_ sessions summarizeSession
