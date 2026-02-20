{-# LANGUAGE TemplateHaskell #-}
module Tui.Types
  ( AppState(..)
  , View(..)
  , ChatState(..)
  , ChatMessage(..)
  , ActivitiesState(..)
  , DocumentsState(..)
  , ApprovalsState(..)
  , DocumentTab(..)
  , Name(..)
  , AppEvent(..)
  -- Lenses
  , currentView
  , chatState
  , activitiesState
  , documentsState
  , approvalsState
  , statusMessage
  , clientConfig
  -- Chat lenses
  , csMessages
  , csInputBuffer
  , csCurrentAgent
  , csCurrentSession
  , csStreaming
  , csStreamBuffer
  -- Activities lenses
  , asActivities
  , asSelected
  , asExpanded
  , asFilter
  -- Documents lenses
  , dsCurrentTab
  , dsProjects
  , dsNotes
  , dsPrefs
  , dsSelected
  -- Approvals lenses
  , apsItems
  , apsSelected
  , apsExpanded
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Lens.Micro.TH (makeLenses)

import Wisp.Client (ClientConfig, Activity, Document)
import Wisp.Client.SSE (ChatEvent)

-- | Resource names for brick
data Name
  = ChatInput
  | ChatHistory
  | ActivityList
  | DocumentList
  | ApprovalList
  deriving (Show, Eq, Ord)

-- | Custom events
data AppEvent
  = ChatEventReceived ChatEvent
  | RefreshView View
  | Tick
  deriving (Show, Eq)

-- | Main views
data View = ChatView | ActivitiesView | DocumentsView | ApprovalsView
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Document sub-tabs
data DocumentTab = ProjectsTab | NotesTab | PrefsTab
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Chat message for display
data ChatMessage = ChatMessage
  { cmRole :: Text
  , cmContent :: Text
  , cmTimestamp :: UTCTime
  } deriving (Show, Eq)

-- | Chat view state
data ChatState = ChatState
  { _csMessages :: [ChatMessage]
  , _csInputBuffer :: Text
  , _csCurrentAgent :: Text
  , _csCurrentSession :: Text
  , _csStreaming :: Bool
  , _csStreamBuffer :: Text
  } deriving (Show)

makeLenses ''ChatState

-- | Activities view state
data ActivitiesState = ActivitiesState
  { _asActivities :: [Activity]
  , _asSelected :: Int
  , _asExpanded :: Maybe Int
  , _asFilter :: Text
  } deriving (Show)

makeLenses ''ActivitiesState

-- | Documents view state
data DocumentsState = DocumentsState
  { _dsCurrentTab :: DocumentTab
  , _dsProjects :: [Document]
  , _dsNotes :: [Document]
  , _dsPrefs :: [Document]
  , _dsSelected :: Int
  } deriving (Show)

makeLenses ''DocumentsState

-- | Approvals view state
data ApprovalsState = ApprovalsState
  { _apsItems :: [(Activity, Text, Text)]  -- (activity, type, reason)
  , _apsSelected :: Int
  , _apsExpanded :: Maybe Int
  } deriving (Show)

makeLenses ''ApprovalsState

-- | Main application state
data AppState = AppState
  { _currentView :: View
  , _chatState :: ChatState
  , _activitiesState :: ActivitiesState
  , _documentsState :: DocumentsState
  , _approvalsState :: ApprovalsState
  , _clientConfig :: ClientConfig
  , _statusMessage :: Maybe (Text, UTCTime)
  }

makeLenses ''AppState
