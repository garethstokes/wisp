module Tui.Views.Approvals
  ( approvalsWidget
  , handleApprovalsEvent
  ) where

import Brick
import Brick.Main (vScrollBy, viewportScroll)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Activity(..), ActivitySource(..), ActivityStatus(..))

-- | Approvals view widget
approvalsWidget :: ApprovalsState -> Widget Name
approvalsWidget as = case as ^. apsExpanded of
  Just idx | idx < length (as ^. apsItems) ->
    -- Show full detail view for expanded approval
    let (act, aType, reason) = (as ^. apsItems) !! idx
    in approvalDetailView act aType reason
  _ -> vBox
    [ queueHeader as
    , hBorder
    , approvalsList as
    , hBorder
    , helpBar
    ]
  where
    hBorder = txt $ T.replicate 80 "─"
    helpBar = padTop (Pad 1) $ txt " [y] approve  [x] dismiss  [l/Enter] expand  [j/k] navigate"

queueHeader :: ApprovalsState -> Widget Name
queueHeader as =
  let count = length (as ^. apsItems)
  in padBottom (Pad 1) $ txt $ " 📋 Review Queue (" <> T.pack (show count) <> " pending)"

approvalsList :: ApprovalsState -> Widget Name
approvalsList as =
  if null (as ^. apsItems)
  then padAll 2 $ txt "No items pending approval. 🎉"
  else viewport ApprovalList Vertical $ vBox $
       zipWith (renderApproval (as ^. apsSelected) (as ^. apsExpanded)) [0..] (as ^. apsItems)

renderApproval :: Int -> Maybe Int -> Int -> (Activity, Text, Text) -> Widget Name
renderApproval selected _expanded idx (act, aType, _reason) =
  let isSelected = idx == selected
      marker = if isSelected then "▶ " else "  "
      icon = sourceIcon (activitySource act)
      title = maybe (T.take 80 $ activityRaw act) id (activityTitle act)
      typeLabel = T.justifyLeft 12 ' ' aType
  in hBox
       [ txt marker
       , withAttr (attrName "type") $ txt typeLabel
       , txt icon
       , txt " "
       , padRight Max $ txt title
       ]

sourceIcon :: ActivitySource -> Text
sourceIcon Email = "📧"
sourceIcon Calendar = "📅"
sourceIcon GitHubEvent = "🐙"
sourceIcon Conversation = "💬"
sourceIcon Note = "📝"

-- | Full detail view for an approval item
approvalDetailView :: Activity -> Text -> Text -> Widget Name
approvalDetailView act aType reason = padAll 1 $ vBox
  [ txt $ "ID: " <> activityId act
  , txt ""
  , withAttr (attrName "title") $ txtWrap $ maybe "(no title)" id (activityTitle act)
  , txt ""
  , txt $ "Type: " <> aType
  , txt $ "Reason: " <> reason
  , txt $ "Source: " <> sourceLabel (activitySource act)
  , txt $ "Status: " <> statusText (activityStatus act)
  , txt $ "Tags: " <> if null (activityTags act) then "(none)" else T.intercalate ", " (activityTags act)
  , txt ""
  , txt "─────────────────────────────────────────"
  , txt ""
  , viewport ApprovalDetail Vertical $ txtWrap (activityRaw act)
  , txt ""
  , txt "Press Esc or 'h' to go back | [y] approve  [x] dismiss"
  ]

sourceLabel :: ActivitySource -> Text
sourceLabel Email = "Email"
sourceLabel Calendar = "Calendar"
sourceLabel GitHubEvent = "GitHub"
sourceLabel Conversation = "Conversation"
sourceLabel Note = "Note"

statusText :: ActivityStatus -> Text
statusText Pending = "pending"
statusText Stored = "stored"
statusText Surfaced = "surfaced"
statusText Quarantined = "quarantine"
statusText Archived = "archived"
statusText NeedsReview = "review"
statusText Processed = "processed"

-- | Handle approvals-specific events
handleApprovalsEvent :: V.Event -> EventM Name AppState ()
handleApprovalsEvent (V.EvKey V.KEsc []) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Just _ -> closeDetail
    Nothing -> pure ()
handleApprovalsEvent (V.EvKey (V.KChar 'h') []) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Just _ -> closeDetail
    Nothing -> pure ()
handleApprovalsEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Nothing -> do
      let maxIdx = length (s ^. approvalsState . apsItems) - 1
      modify $ approvalsState . apsSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
      vScrollBy (viewportScroll ApprovalList) 1
    Just _ -> vScrollBy (viewportScroll ApprovalDetail) 1
handleApprovalsEvent (V.EvKey (V.KChar 'k') []) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Nothing -> do
      modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
      vScrollBy (viewportScroll ApprovalList) (-1)
    Just _ -> vScrollBy (viewportScroll ApprovalDetail) (-1)
handleApprovalsEvent (V.EvKey V.KDown []) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Nothing -> do
      let maxIdx = length (s ^. approvalsState . apsItems) - 1
      modify $ approvalsState . apsSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
      vScrollBy (viewportScroll ApprovalList) 1
    Just _ -> vScrollBy (viewportScroll ApprovalDetail) 1
handleApprovalsEvent (V.EvKey V.KUp []) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Nothing -> do
      modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
      vScrollBy (viewportScroll ApprovalList) (-1)
    Just _ -> vScrollBy (viewportScroll ApprovalDetail) (-1)
handleApprovalsEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Nothing -> do
      let maxIdx = length (s ^. approvalsState . apsItems) - 1
      modify $ approvalsState . apsSelected %~ (\i -> min (i + 15) (max 0 maxIdx))
      vScrollBy (viewportScroll ApprovalList) 15
    Just _ -> vScrollBy (viewportScroll ApprovalDetail) 15
handleApprovalsEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) = do
  s <- get
  case s ^. approvalsState . apsExpanded of
    Nothing -> do
      modify $ approvalsState . apsSelected %~ (\i -> max (i - 15) 0)
      vScrollBy (viewportScroll ApprovalList) (-15)
    Just _ -> vScrollBy (viewportScroll ApprovalDetail) (-15)
handleApprovalsEvent (V.EvKey (V.KChar 'l') []) = openDetail
handleApprovalsEvent (V.EvKey V.KEnter []) = openDetail
handleApprovalsEvent (V.EvKey (V.KChar 'y') []) = do
  -- Approve selected item (will be wired up in Task 13)
  s <- get
  let sel = s ^. approvalsState . apsSelected
  modify $ approvalsState . apsItems %~ removeAt sel
  modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
  modify $ approvalsState . apsExpanded .~ Nothing
handleApprovalsEvent (V.EvKey (V.KChar 'x') []) = do
  -- Dismiss selected item
  s <- get
  let sel = s ^. approvalsState . apsSelected
  modify $ approvalsState . apsItems %~ removeAt sel
  modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
  modify $ approvalsState . apsExpanded .~ Nothing
handleApprovalsEvent _ = pure ()

openDetail :: EventM Name AppState ()
openDetail = do
  s <- get
  let sel = s ^. approvalsState . apsSelected
  modify $ approvalsState . apsExpanded .~ Just sel

closeDetail :: EventM Name AppState ()
closeDetail = modify $ approvalsState . apsExpanded .~ Nothing

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs
