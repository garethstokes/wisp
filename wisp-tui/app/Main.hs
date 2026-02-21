module Main where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void, when)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((%~), (^.), (.~))

import qualified Tui.DataLoader as DL
import Wisp.Client.SSE (ChatEvent(..))
import Tui.Types
import Tui.Views.Activities (activitiesWidget, handleActivitiesEvent, handleActivitiesEventWithAction, ActivitiesAction(..))
import Tui.Views.Agents (agentsWidget, handleAgentsEvent)
import Tui.Views.Approvals (approvalsWidget, handleApprovalsEvent)
import Tui.Views.Chat (chatWidget, handleChatEvent, sendChatMessage)
import Tui.Views.Knowledge (knowledgeWidget, handleKnowledgeEvent)
import Tui.Views.Skills (skillsWidget, handleSkillsEvent)
import Tui.Widgets.Layout (headerWidget, statusBarWidget)
import Wisp.Client (defaultConfig, AgentInfo(..))

main :: IO ()
main = do
  -- Create event channel
  chan <- newBChan 10

  -- Start tick thread
  void $ forkIO $ forever $ do
    threadDelay 1000000  -- 1 second
    writeBChan chan Tick

  -- Build initial state
  now <- getCurrentTime
  let initialState = AppState
        { _currentView = ChatView
        , _chatState = ChatState
            { _csMessages = []
            , _csInputBuffer = ""
            , _csCurrentAgent = "wisp/concierge"
            , _csCurrentSession = "default"
            , _csStreaming = False
            , _csStreamBuffer = ""
            }
        , _activitiesState = ActivitiesState [] 0 Nothing "" Nothing True False
        , _knowledgeState = KnowledgeState NotesTab [] [] 0 Nothing
        , _skillsState = SkillsState [] 0 Nothing
        , _agentsState = AgentsState [] 0 Nothing []
        , _approvalsState = ApprovalsState [] 0 Nothing
        , _clientConfig = defaultConfig
        , _statusMessage = Just ("Welcome to wisp-tui | Ctrl-Q to quit", now, StatusInfo)
        , _currentTime = Just now
        }

  -- Build vty
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run app
  void $ customMain initialVty buildVty (Just chan) (app chan) initialState

app :: BChan AppEvent -> App AppState AppEvent Name
app chan = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent chan
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "selectedTab", V.withStyle V.defAttr V.bold)
  , (attrName "connected", fg V.green)
  , (attrName "userRole", fg V.cyan)
  , (attrName "assistantRole", fg V.magenta)
  , (attrName "cursor", V.withStyle V.defAttr V.blink)
  , (attrName "error", fg V.red `V.withStyle` V.bold)
  ]

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ vBox
      [ headerWidget (s ^. currentView) (s ^. chatState . csCurrentAgent)
      , hBorder
      , viewContent s
      , hBorder
      , statusBarWidget ((\(msg, _, sev) -> (msg, sev)) <$> s ^. statusMessage)
      ]
  ]

viewContent :: AppState -> Widget Name
viewContent s = case s ^. currentView of
  ChatView -> chatWidget (s ^. chatState)
  ActivitiesView -> activitiesWidget (s ^. currentTime) (s ^. activitiesState)
  KnowledgeView -> knowledgeWidget (s ^. knowledgeState)
  SkillsView -> skillsWidget (s ^. skillsState)
  AgentsView -> agentsWidget (s ^. agentsState)
  ApprovalsView -> approvalsWidget (s ^. approvalsState)

handleEvent :: BChan AppEvent -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
handleEvent chan (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  modify $ currentView %~ nextView
  triggerDataLoad chan
handleEvent chan (VtyEvent (V.EvKey V.KBackTab [])) = do
  modify $ currentView %~ prevView
  triggerDataLoad chan
handleEvent chan (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  s <- get
  -- 'r' refreshes in non-chat views, but in chat view it's a normal character
  case s ^. currentView of
    ChatView -> handleChatEvent (V.EvKey (V.KChar 'r') [])
    _ -> triggerDataLoad chan
handleEvent _ (AppEvent (ActivitiesLoaded acts metrics hasMore)) = do
  modify $ activitiesState . asActivities .~ acts
  modify $ activitiesState . asSelected .~ 0
  modify $ activitiesState . asMetrics .~ metrics
  modify $ activitiesState . asHasMore .~ hasMore
  modify $ activitiesState . asLoading .~ False
handleEvent _ (AppEvent (ActivitiesAppended acts hasMore)) = do
  modify $ activitiesState . asActivities %~ (++ acts)
  modify $ activitiesState . asHasMore .~ hasMore
  modify $ activitiesState . asLoading .~ False
handleEvent _ (AppEvent (KnowledgeLoaded notes prefs)) = do
  modify $ knowledgeState . ksNotes .~ notes
  modify $ knowledgeState . ksPrefs .~ prefs
  modify $ knowledgeState . ksSelected .~ 0
handleEvent _ (AppEvent (SkillsLoaded skills)) = do
  modify $ skillsState . ssSkills .~ skills
  modify $ skillsState . ssSelected .~ 0
handleEvent _ (AppEvent (AgentsLoaded agents)) = do
  modify $ agentsState . agsAgents .~ agents
  modify $ agentsState . agsSelected .~ 0
handleEvent _ (AppEvent (AgentSessionsLoaded _name sessions)) = do
  modify $ agentsState . agsSessions .~ sessions
handleEvent _ (AppEvent (ApprovalsLoaded items)) = do
  modify $ approvalsState . apsItems .~ items
  modify $ approvalsState . apsSelected .~ 0
handleEvent _ (AppEvent (LoadError _msg)) = do
  -- Could show error in status bar
  pure ()
handleEvent _ (AppEvent (RefreshView _)) = do
  pure ()
handleEvent _ (AppEvent Tick) = do
  now <- liftIO getCurrentTime
  modify $ currentTime .~ Just now
handleEvent _ (AppEvent (ChatEventReceived evt)) = do
  -- Handle chat streaming events
  handleSSEEvent evt
handleEvent chan (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  case s ^. currentView of
    ChatView -> handleChatEnter chan
    ActivitiesView -> handleActivitiesEvent (V.EvKey V.KEnter [])
    KnowledgeView -> handleKnowledgeEvent (V.EvKey V.KEnter [])
    SkillsView -> handleSkillsEvent (V.EvKey V.KEnter [])
    AgentsView -> handleAgentsEnter chan
    ApprovalsView -> handleApprovalsEvent (V.EvKey V.KEnter [])
handleEvent chan (VtyEvent e) = do
  s <- get
  case s ^. currentView of
    ChatView -> handleChatEvent e
    ActivitiesView -> do
      action <- handleActivitiesEventWithAction e
      case action of
        LoadMore -> triggerLoadMoreActivities chan
        NoAction -> pure ()
    KnowledgeView -> handleKnowledgeEvent e
    SkillsView -> handleSkillsEvent e
    AgentsView -> handleAgentsEvent e
    ApprovalsView -> handleApprovalsEvent e
handleEvent _ _ = pure ()

-- | Trigger async data load for current view
triggerDataLoad :: BChan AppEvent -> EventM Name AppState ()
triggerDataLoad chan = do
  s <- get
  let cfg = s ^. clientConfig
      view = s ^. currentView

  -- Fork async load based on view
  void $ liftIO $ async $ do
    result <- case view of
      ActivitiesView -> DL.loadActivities cfg
      KnowledgeView -> DL.loadKnowledge cfg
      SkillsView -> DL.loadSkills cfg
      AgentsView -> DL.loadAgents cfg
      ApprovalsView -> DL.loadApprovals cfg
      ChatView -> pure $ DL.LoadError "Chat doesn't need loading"

    -- Send result back via channel
    case result of
      DL.ActivitiesLoaded acts metrics hasMore ->
        writeBChan chan (ActivitiesLoaded acts metrics hasMore)
      DL.ActivitiesAppended acts hasMore ->
        writeBChan chan (ActivitiesAppended acts hasMore)
      DL.KnowledgeLoaded notes prefs ->
        writeBChan chan (KnowledgeLoaded notes prefs)
      DL.SkillsLoaded skills ->
        writeBChan chan (SkillsLoaded skills)
      DL.AgentsLoaded agents ->
        writeBChan chan (AgentsLoaded agents)
      DL.AgentSessionsLoaded name sessions ->
        writeBChan chan (AgentSessionsLoaded name sessions)
      DL.ApprovalsLoaded items ->
        writeBChan chan (ApprovalsLoaded items)
      DL.LoadError msg ->
        writeBChan chan (LoadError msg)

-- | Trigger loading more activities (pagination)
triggerLoadMoreActivities :: BChan AppEvent -> EventM Name AppState ()
triggerLoadMoreActivities chan = do
  s <- get
  let cfg = s ^. clientConfig
      currentCount = length (s ^. activitiesState . asActivities)
  void $ liftIO $ async $ do
    result <- DL.loadMoreActivities cfg currentCount
    case result of
      DL.ActivitiesAppended acts hasMore ->
        writeBChan chan (ActivitiesAppended acts hasMore)
      DL.LoadError msg ->
        writeBChan chan (LoadError msg)
      _ -> pure ()

nextView :: View -> View
nextView ChatView = ActivitiesView
nextView ActivitiesView = KnowledgeView
nextView KnowledgeView = SkillsView
nextView SkillsView = AgentsView
nextView AgentsView = ApprovalsView
nextView ApprovalsView = ChatView

prevView :: View -> View
prevView ChatView = ApprovalsView
prevView ActivitiesView = ChatView
prevView KnowledgeView = ActivitiesView
prevView SkillsView = KnowledgeView
prevView AgentsView = SkillsView
prevView ApprovalsView = AgentsView

-- | Handle Enter key in agents view - load sessions when expanding
handleAgentsEnter :: BChan AppEvent -> EventM Name AppState ()
handleAgentsEnter chan = do
  s <- get
  let idx = s ^. agentsState . agsSelected
      agents = s ^. agentsState . agsAgents
  case s ^. agentsState . agsExpanded of
    Just _ -> do
      -- Already expanded, collapse
      modify $ agentsState . agsExpanded .~ Nothing
      modify $ agentsState . agsSessions .~ []
    Nothing -> do
      -- Expand and load sessions
      modify $ agentsState . agsExpanded .~ Just idx
      when (idx < length agents) $ do
        let agent = agents !! idx
            cfg = s ^. clientConfig
        void $ liftIO $ async $ do
          result <- DL.loadAgentSessions cfg (agentName agent)
          case result of
            DL.AgentSessionsLoaded name sessions ->
              writeBChan chan (AgentSessionsLoaded name sessions)
            _ -> pure ()

-- | Handle Enter key in chat view
handleChatEnter :: BChan AppEvent -> EventM Name AppState ()
handleChatEnter chan = do
  s <- get
  let input = s ^. chatState . csInputBuffer
      isStreaming = s ^. chatState . csStreaming
  if isStreaming || T.null input
    then pure ()
    else do
      -- Add user message
      now <- liftIO getCurrentTime
      let userMsg = ChatMessage "You" input now
          messagesForRequest = (s ^. chatState . csMessages) ++ [userMsg]
      modify $ chatState . csMessages .~ messagesForRequest
      modify $ chatState . csInputBuffer .~ ""
      modify $ chatState . csStreaming .~ True
      modify $ chatState . csStreamBuffer .~ ""

      -- Send to server
      let cfg = s ^. clientConfig
          agent = s ^. chatState . csCurrentAgent
          session = s ^. chatState . csCurrentSession
      liftIO $ sendChatMessage cfg agent session messagesForRequest chan

-- | Handle SSE events from chat streaming
handleSSEEvent :: ChatEvent -> EventM Name AppState ()
handleSSEEvent (ChunkEvent chunk) = do
  modify $ chatState . csStreamBuffer %~ (<> chunk)
handleSSEEvent (ToolCallStart name) = do
  now <- liftIO getCurrentTime
  modify $ statusMessage .~ Just ("Calling " <> name <> "...", now, StatusInfo)
handleSSEEvent (ToolCallResult name ms) = do
  now <- liftIO getCurrentTime
  modify $ statusMessage .~ Just (name <> " completed in " <> T.pack (show ms) <> "ms", now, StatusInfo)
handleSSEEvent (DoneEvent _ _) = do
  s <- get
  now <- liftIO getCurrentTime
  let response = s ^. chatState . csStreamBuffer
  if T.null response
    then pure ()
    else do
      let assistantMsg = ChatMessage "Assistant" response now
      modify $ chatState . csMessages %~ (++ [assistantMsg])
  modify $ chatState . csStreamBuffer .~ ""
  modify $ chatState . csStreaming .~ False
handleSSEEvent (ErrorEvent msg _) = do
  now <- liftIO getCurrentTime
  modify $ statusMessage .~ Just (msg, now, StatusError)
  modify $ chatState . csStreamBuffer .~ ""
  modify $ chatState . csStreaming .~ False
