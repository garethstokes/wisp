module Tui.Views.Approvals
  ( approvalsWidget
  , handleApprovalsEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Activity(..), ActivitySource(..))

-- | Approvals view widget
approvalsWidget :: ApprovalsState -> Widget Name
approvalsWidget as = vBox
  [ queueHeader as
  , approvalsList as
  ]

queueHeader :: ApprovalsState -> Widget Name
queueHeader as =
  let count = length (as ^. apsItems)
  in vLimit 1 $ txt $ " Review Queue (" <> T.pack (show count) <> " pending)"

approvalsList :: ApprovalsState -> Widget Name
approvalsList as = vBox $
  [ padBottom (Pad 1) $ txt "  Type        Activity                        Reason"
  , txt "  ------"
  ] ++ if null (as ^. apsItems)
       then [padAll 1 $ txt "No items pending approval."]
       else zipWith (renderApproval (as ^. apsSelected) (as ^. apsExpanded)) [0..] (as ^. apsItems)

renderApproval :: Int -> Maybe Int -> Int -> (Activity, Text, Text) -> Widget Name
renderApproval selected expanded idx (act, aType, reason) =
  let isSelected = idx == selected
      isExpanded = expanded == Just idx
      marker = if isSelected then "> " else "  "
      icon = sourceIcon (activitySource act)
      title = maybe (T.take 30 $ activityRaw act) (T.take 30) (activityTitle act)
      baseRow = hBox
        [ txt marker
        , txt $ T.justifyLeft 10 ' ' aType
        , txt icon
        , txt " "
        , txt $ T.justifyLeft 30 ' ' title
        , txt reason
        ]
  in if isExpanded
     then vBox [baseRow, expandedApproval act aType]
     else baseRow

sourceIcon :: ActivitySource -> Text
sourceIcon Email = "[E]"
sourceIcon Calendar = "[C]"
sourceIcon GitHubEvent = "[G]"
sourceIcon Conversation = "[T]"
sourceIcon Note = "[N]"

expandedApproval :: Activity -> Text -> Widget Name
expandedApproval act aType = padLeft (Pad 4) $ vBox
  [ txt $ "Tags: " <> T.intercalate ", " (activityTags act)
  , txt "------"
  , txtWrap $ T.take 200 (activityRaw act)
  , txt ""
  , txt $ "[y:approve  x:dismiss" <> (if aType == "classify" then "  c:change category" else "") <> "]"
  ]

-- | Handle approvals-specific events
handleApprovalsEvent :: V.Event -> EventM Name AppState ()
handleApprovalsEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (s ^. approvalsState . apsItems) - 1
  modify $ approvalsState . apsSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
handleApprovalsEvent (V.EvKey (V.KChar 'k') []) =
  modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
handleApprovalsEvent (V.EvKey (V.KChar 'l') []) = toggleExpand
handleApprovalsEvent (V.EvKey V.KEnter []) = toggleExpand
handleApprovalsEvent (V.EvKey (V.KChar 'y') []) = do
  -- Approve selected item (will be wired up in Task 13)
  s <- get
  let sel = s ^. approvalsState . apsSelected
  modify $ approvalsState . apsItems %~ removeAt sel
  modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
handleApprovalsEvent (V.EvKey (V.KChar 'x') []) = do
  -- Dismiss selected item
  s <- get
  let sel = s ^. approvalsState . apsSelected
  modify $ approvalsState . apsItems %~ removeAt sel
  modify $ approvalsState . apsSelected %~ (\i -> max 0 (i - 1))
handleApprovalsEvent _ = pure ()

toggleExpand :: EventM Name AppState ()
toggleExpand = do
  s <- get
  let sel = s ^. approvalsState . apsSelected
      expanded = s ^. approvalsState . apsExpanded
  modify $ approvalsState . apsExpanded .~ (if expanded == Just sel then Nothing else Just sel)

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs
