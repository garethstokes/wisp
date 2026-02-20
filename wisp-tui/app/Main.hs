module Main where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((%~), (^.), (.~))

import Tui.DataLoader (loadActivities, loadDocuments, loadApprovals, DataLoadResult(..))
import Tui.Types
import Tui.Views.Activities (activitiesWidget, handleActivitiesEvent)
import Tui.Views.Approvals (approvalsWidget, handleApprovalsEvent)
import Tui.Views.Chat (chatWidget, handleChatEvent)
import Tui.Views.Documents (documentsWidget, handleDocumentsEvent)
import Tui.Widgets.Layout (headerWidget, statusBarWidget)
import Wisp.Client (defaultConfig)

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
        , _activitiesState = ActivitiesState [] 0 Nothing ""
        , _documentsState = DocumentsState ProjectsTab [] [] [] 0
        , _approvalsState = ApprovalsState [] 0 Nothing
        , _clientConfig = defaultConfig
        , _statusMessage = Just ("Welcome to wisp-tui | Ctrl-Q to quit", now)
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
  ]

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ vBox
      [ headerWidget (s ^. currentView) (s ^. chatState . csCurrentAgent)
      , hBorder
      , viewContent s
      , hBorder
      , statusBarWidget (fst <$> s ^. statusMessage)
      ]
  ]

viewContent :: AppState -> Widget Name
viewContent s = case s ^. currentView of
  ChatView -> chatWidget (s ^. chatState)
  ActivitiesView -> activitiesWidget (s ^. activitiesState)
  DocumentsView -> documentsWidget (s ^. documentsState)
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
  -- Refresh current view
  triggerDataLoad chan
handleEvent _ (AppEvent (RefreshView _)) = do
  -- Placeholder for when we receive loaded data
  pure ()
handleEvent _ (AppEvent Tick) = pure ()
handleEvent chan (AppEvent (ChatEventReceived evt)) = do
  -- Handle chat streaming events (wired up in Task 14)
  pure ()
handleEvent _ (VtyEvent e) = do
  s <- get
  case s ^. currentView of
    ChatView -> handleChatEvent e
    ActivitiesView -> handleActivitiesEvent e
    DocumentsView -> handleDocumentsEvent e
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
      ActivitiesView -> loadActivities cfg
      DocumentsView -> loadDocuments cfg
      ApprovalsView -> loadApprovals cfg
      ChatView -> pure $ LoadError "Chat doesn't need loading"

    -- Send result back via channel
    case result of
      ActivitiesLoaded _ ->
        writeBChan chan (RefreshView ActivitiesView)
      DocumentsLoaded {} ->
        writeBChan chan (RefreshView DocumentsView)
      ApprovalsLoaded _ ->
        writeBChan chan (RefreshView ApprovalsView)
      LoadError _ -> pure ()

nextView :: View -> View
nextView ChatView = ActivitiesView
nextView ActivitiesView = DocumentsView
nextView DocumentsView = ApprovalsView
nextView ApprovalsView = ChatView

prevView :: View -> View
prevView ChatView = ApprovalsView
prevView ActivitiesView = ChatView
prevView DocumentsView = ActivitiesView
prevView ApprovalsView = DocumentsView
