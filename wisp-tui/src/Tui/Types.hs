{-# LANGUAGE TemplateHaskell #-}
module Tui.Types
  ( AppState(..)
  , View(..)
  , ChatState(..)
  , ChatMessage(..)
  , ActivitiesState(..)
  , KnowledgeState(..)
  , SkillsState(..)
  , AgentsState(..)
  , ApprovalsState(..)
  , KnowledgeTab(..)
  , Name(..)
  , AppEvent(..)
  , StatusSeverity(..)
  -- Lenses
  , currentView
  , chatState
  , activitiesState
  , knowledgeState
  , skillsState
  , agentsState
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
  , csToolCalls
  -- Activities lenses
  , asActivities
  , asSelected
  , asExpanded
  , asFilter
  , asMetrics
  , asHasMore
  , asLoading
  -- Knowledge lenses
  , ksCurrentTab
  , ksProjects
  , ksNotes
  , ksPrefs
  , ksSelected
  , ksExpanded
  , ksProjectChildren
  -- Skills lenses
  , ssSkills
  , ssSelected
  , ssExpanded
  -- Agents lenses
  , agsAgents
  , agsSelected
  , agsExpanded
  , agsSessions
  -- Approvals lenses
  , apsItems
  , apsSuggestions
  , apsSelected
  , apsExpanded
  -- Time lens
  , currentTime
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Lens.Micro.TH (makeLenses)

import Wisp.Client (ClientConfig, Activity, ActivityMetrics, Document, Skill, AgentInfo, SessionSummary, ProjectSuggestion)
import qualified Wisp.Client as WC
import Wisp.Client.SSE (ChatEvent)

-- | Resource names for brick
data Name
  = ChatInput
  | ChatHistory
  | ActivityList
  | ActivityDetail  -- Separate viewport for activity detail view
  | KnowledgeList
  | KnowledgeDetail
  | SkillsList
  | SkillsDetail
  | AgentsList
  | AgentsDetail
  | ApprovalList
  | ApprovalDetail  -- Separate viewport for approval detail view
  deriving (Show, Eq, Ord)

-- | Custom events
data AppEvent
  = ChatEventReceived ChatEvent
  | ActivitiesLoaded [Activity] (Maybe ActivityMetrics) Bool  -- activities, metrics, hasMore
  | ActivitiesAppended [Activity] Bool  -- more activities, hasMore
  | KnowledgeLoaded [Document] [Document] [Document]  -- projects, notes, prefs
  | ProjectChildrenLoaded [Document]  -- children of a project
  | SkillsLoaded [Skill]
  | AgentsLoaded [AgentInfo]
  | AgentSessionsLoaded Text [SessionSummary]  -- agent name, sessions
  | ChatSessionLoaded (Maybe (Text, [WC.ChatMessage]))  -- sessionId, messages
  | ApprovalsLoaded [(Activity, Text, Text)] [ProjectSuggestion]
  | LoadError Text
  | RefreshView View
  | Tick
  deriving (Show, Eq)

-- | Status message severity
data StatusSeverity = StatusInfo | StatusError
  deriving (Show, Eq)

-- | Main views
data View = ChatView | ActivitiesView | KnowledgeView | SkillsView | AgentsView | ApprovalsView
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Knowledge sub-tabs
data KnowledgeTab = ProjectsTab | NotesTab | PrefsTab
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
  , _csToolCalls :: [(Text, Maybe Int)]  -- (name, duration_ms or Nothing if pending)
  } deriving (Show)

makeLenses ''ChatState

-- | Activities view state
data ActivitiesState = ActivitiesState
  { _asActivities :: [Activity]
  , _asSelected :: Int
  , _asExpanded :: Maybe Int
  , _asFilter :: Text
  , _asMetrics :: Maybe ActivityMetrics
  , _asHasMore :: Bool
  , _asLoading :: Bool
  } deriving (Show)

makeLenses ''ActivitiesState

-- | Knowledge view state (renamed from Documents)
data KnowledgeState = KnowledgeState
  { _ksCurrentTab :: KnowledgeTab
  , _ksProjects :: [Document]
  , _ksNotes :: [Document]
  , _ksPrefs :: [Document]
  , _ksSelected :: Int
  , _ksExpanded :: Maybe Int
  , _ksProjectChildren :: [Document]  -- Children of currently expanded project
  } deriving (Show)

makeLenses ''KnowledgeState

-- | Skills view state
data SkillsState = SkillsState
  { _ssSkills :: [Skill]
  , _ssSelected :: Int
  , _ssExpanded :: Maybe Int
  } deriving (Show)

makeLenses ''SkillsState

-- | Agents view state
data AgentsState = AgentsState
  { _agsAgents :: [AgentInfo]
  , _agsSelected :: Int
  , _agsExpanded :: Maybe Int
  , _agsSessions :: [SessionSummary]  -- Sessions for currently expanded agent
  } deriving (Show)

makeLenses ''AgentsState

-- | Approvals view state
data ApprovalsState = ApprovalsState
  { _apsItems :: [(Activity, Text, Text)]  -- (activity, type, reason)
  , _apsSuggestions :: [ProjectSuggestion]  -- new project suggestions
  , _apsSelected :: Int
  , _apsExpanded :: Maybe Int
  } deriving (Show)

makeLenses ''ApprovalsState

-- | Main application state
data AppState = AppState
  { _currentView :: View
  , _chatState :: ChatState
  , _activitiesState :: ActivitiesState
  , _knowledgeState :: KnowledgeState
  , _skillsState :: SkillsState
  , _agentsState :: AgentsState
  , _approvalsState :: ApprovalsState
  , _clientConfig :: ClientConfig
  , _statusMessage :: Maybe (Text, UTCTime, StatusSeverity)
  , _currentTime :: Maybe UTCTime
  }

makeLenses ''AppState
