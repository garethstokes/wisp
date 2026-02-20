module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((%~), (^.))

import Tui.Types
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
        , _statusMessage = Just ("Welcome to wisp-tui", now)
        }

  -- Build vty
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run app
  void $ customMain initialVty buildVty (Just chan) app initialState

app :: App AppState AppEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "selectedTab", V.withStyle V.defAttr V.bold)
  , (attrName "connected", fg V.green)
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
viewContent s = padAll 1 $ case s ^. currentView of
  ChatView -> str "Chat view - Press 'q' to quit"
  ActivitiesView -> str "Activities view"
  DocumentsView -> str "Documents view"
  ApprovalsView -> str "Approvals view"

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  modify $ currentView %~ nextView
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
  modify $ currentView %~ prevView
handleEvent _ = pure ()

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
