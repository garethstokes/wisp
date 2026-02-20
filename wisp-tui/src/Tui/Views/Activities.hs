module Tui.Views.Activities
  ( activitiesWidget
  , handleActivitiesEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Activity(..), ActivitySource(..), ActivityStatus(..))

-- | Activities view widget
activitiesWidget :: ActivitiesState -> Widget Name
activitiesWidget as = vBox
  [ filterBar as
  , activityList as
  ]

filterBar :: ActivitiesState -> Widget Name
filterBar as = vLimit 1 $ hBox
  [ txt " Filter: "
  , txt $ if T.null (as ^. asFilter) then "all" else as ^. asFilter
  , fill ' '
  , txt "[/]search "
  ]

activityList :: ActivitiesState -> Widget Name
activityList as =
  let activities = as ^. asActivities
      selected = as ^. asSelected
  in if null activities
     then padAll 1 $ txt "No activities. Press 'r' to refresh."
     else viewport ActivityList Vertical $ vBox $
          zipWith (renderActivityRow selected (as ^. asExpanded)) [0..] activities

renderActivityRow :: Int -> Maybe Int -> Int -> Activity -> Widget Name
renderActivityRow selected expanded idx act =
  let isSelected = idx == selected
      isExpanded = expanded == Just idx
      marker = if isSelected then "> " else "  "
      icon = sourceIcon (activitySource act)
      title = maybe (T.take 40 $ activityRaw act) id (activityTitle act)
      status = statusText (activityStatus act)
      baseRow = hBox
        [ txt marker
        , txt icon
        , txt " "
        , txt $ T.take 35 title
        , fill ' '
        , txt status
        ]
  in if isExpanded
     then vBox [baseRow, expandedContent act]
     else baseRow

sourceIcon :: ActivitySource -> Text
sourceIcon Email = "[E]"
sourceIcon Calendar = "[C]"
sourceIcon GitHubEvent = "[G]"
sourceIcon Conversation = "[T]"
sourceIcon Note = "[N]"

statusText :: ActivityStatus -> Text
statusText Pending = "pending"
statusText Stored = "stored"
statusText Surfaced = "surfaced"
statusText Quarantined = "quarantine"
statusText Archived = "archived"

expandedContent :: Activity -> Widget Name
expandedContent act = padLeft (Pad 4) $ vBox
  [ txt $ "Tags: " <> T.intercalate ", " (activityTags act)
  , txt "------"
  , txtWrap $ T.take 200 (activityRaw act)
  ]

-- | Handle activities-specific events
handleActivitiesEvent :: V.Event -> EventM Name AppState ()
handleActivitiesEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (s ^. activitiesState . asActivities) - 1
  modify $ activitiesState . asSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
handleActivitiesEvent (V.EvKey (V.KChar 'k') []) = do
  modify $ activitiesState . asSelected %~ (\i -> max (i - 1) 0)
handleActivitiesEvent (V.EvKey (V.KChar 'l') []) = toggleExpand
handleActivitiesEvent (V.EvKey V.KEnter []) = toggleExpand
handleActivitiesEvent _ = pure ()

toggleExpand :: EventM Name AppState ()
toggleExpand = do
  s <- get
  let sel = s ^. activitiesState . asSelected
      expanded = s ^. activitiesState . asExpanded
  modify $ activitiesState . asExpanded .~ (if expanded == Just sel then Nothing else Just sel)
