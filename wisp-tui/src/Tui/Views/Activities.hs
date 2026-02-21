module Tui.Views.Activities
  ( activitiesWidget
  , handleActivitiesEvent
  ) where

import Brick
import Brick.Main (vScrollBy, viewportScroll)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, NominalDiffTime)
import Data.Time.Clock (nominalDay)
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Wisp.Client (Activity(..), ActivitySource(..), ActivityStatus(..))

-- | Activities view widget
activitiesWidget :: Maybe UTCTime -> ActivitiesState -> Widget Name
activitiesWidget mNow as = case as ^. asExpanded of
  Just idx | idx < length activities ->
    -- Show detail view for expanded activity
    detailView (activities !! idx)
  _ -> vBox
    [ filterBar as
    , activityList mNow as
    ]
  where
    activities = as ^. asActivities

filterBar :: ActivitiesState -> Widget Name
filterBar as = vLimit 1 $ hBox
  [ txt " Filter: "
  , txt $ if T.null (as ^. asFilter) then "all" else as ^. asFilter
  , fill ' '
  , txt "[/]search "
  ]

activityList :: Maybe UTCTime -> ActivitiesState -> Widget Name
activityList mNow as =
  let activities = as ^. asActivities
      selected = as ^. asSelected
  in if null activities
     then padAll 1 $ txt "No activities. Press 'r' to refresh."
     else viewport ActivityList Vertical $ vBox $
          zipWith (renderActivityRow mNow selected) [0..] activities

renderActivityRow :: Maybe UTCTime -> Int -> Int -> Activity -> Widget Name
renderActivityRow mNow selected idx act =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      icon = sourceIcon (activitySource act)
      title = maybe (T.take 80 $ activityRaw act) id (activityTitle act)
      timeAgo = maybe "" (relativeTime (activityCreatedAt act)) mNow
      -- Use padRight Max instead of fill to avoid infinite width
  in hBox
        [ txt marker
        , txt icon
        , txt " "
        , padRight Max $ txt title  -- No truncation, let it use available space
        , txt $ " " <> timeAgo <> " "
        ]

sourceIcon :: ActivitySource -> Text
sourceIcon Email = "📧"
sourceIcon Calendar = "📅"
sourceIcon GitHubEvent = "🐙"
sourceIcon Conversation = "💬"
sourceIcon Note = "📝"

statusText :: ActivityStatus -> Text
statusText Pending = "pending"
statusText Stored = "stored"
statusText Surfaced = "surfaced"
statusText Quarantined = "quarantine"
statusText Archived = "archived"
statusText NeedsReview = "review"
statusText Processed = "processed"

-- | Full detail view for an activity (press 'l' or Enter to view, Esc to go back)
detailView :: Activity -> Widget Name
detailView act = padAll 1 $ vBox
  [ txt $ "ID: " <> activityId act
  , txt ""
  , withAttr (attrName "title") $ txtWrap $ maybe "(no title)" id (activityTitle act)
  , txt ""
  , txt $ "Source: " <> sourceLabel (activitySource act)
  , txt $ "Status: " <> statusText (activityStatus act)
  , txt $ "Tags: " <> if null (activityTags act) then "(none)" else T.intercalate ", " (activityTags act)
  , maybe emptyWidget (\c -> txt $ "Confidence: " <> T.pack (show (round (c * 100) :: Int)) <> "%") (activityConfidence act)
  , txt ""
  , txt "─────────────────────────────────────────"
  , txt ""
  , viewport ActivityDetail Vertical $ txtWrap (activityRaw act)
  , txt ""
  , txt "Press Esc or 'h' to go back"
  ]

sourceLabel :: ActivitySource -> Text
sourceLabel Email = "Email"
sourceLabel Calendar = "Calendar"
sourceLabel GitHubEvent = "GitHub"
sourceLabel Conversation = "Conversation"
sourceLabel Note = "Note"

-- | Format time as relative (e.g., "5m ago", "2h ago", "1d ago")
relativeTime :: UTCTime -> UTCTime -> Text
relativeTime created now =
  let diff = diffUTCTime now created
  in formatDiff diff

formatDiff :: NominalDiffTime -> Text
formatDiff diff
  | diff < 60 = "just now"
  | diff < 3600 = T.pack (show (round (diff / 60) :: Int)) <> "m"
  | diff < nominalDay = T.pack (show (round (diff / 3600) :: Int)) <> "h"
  | diff < nominalDay * 7 = T.pack (show (round (diff / nominalDay) :: Int)) <> "d"
  | otherwise = T.pack (show (round (diff / nominalDay / 7) :: Int)) <> "w"

-- | Handle activities-specific events
handleActivitiesEvent :: V.Event -> EventM Name AppState ()
handleActivitiesEvent (V.EvKey V.KEsc []) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Just _ -> closeDetail
    Nothing -> pure ()
handleActivitiesEvent (V.EvKey (V.KChar 'h') []) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Just _ -> closeDetail
    Nothing -> pure ()  -- Don't do anything with 'h' in list view
handleActivitiesEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Nothing -> do
      -- Navigate and scroll list
      let maxIdx = length (s ^. activitiesState . asActivities) - 1
      modify $ activitiesState . asSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
      vScrollBy (viewportScroll ActivityList) 1
    Just _ ->
      -- Scroll down in detail view
      vScrollBy (viewportScroll ActivityDetail) 1
handleActivitiesEvent (V.EvKey (V.KChar 'k') []) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Nothing -> do
      modify $ activitiesState . asSelected %~ (\i -> max (i - 1) 0)
      vScrollBy (viewportScroll ActivityList) (-1)
    Just _ ->
      -- Scroll up in detail view
      vScrollBy (viewportScroll ActivityDetail) (-1)
handleActivitiesEvent (V.EvKey V.KDown []) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Nothing -> do
      let maxIdx = length (s ^. activitiesState . asActivities) - 1
      modify $ activitiesState . asSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
      vScrollBy (viewportScroll ActivityList) 1
    Just _ -> vScrollBy (viewportScroll ActivityDetail) 1
handleActivitiesEvent (V.EvKey V.KUp []) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Nothing -> do
      modify $ activitiesState . asSelected %~ (\i -> max (i - 1) 0)
      vScrollBy (viewportScroll ActivityList) (-1)
    Just _ -> vScrollBy (viewportScroll ActivityDetail) (-1)
handleActivitiesEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Nothing -> do
      -- Page down in list view
      let maxIdx = length (s ^. activitiesState . asActivities) - 1
      modify $ activitiesState . asSelected %~ (\i -> min (i + 15) (max 0 maxIdx))
      vScrollBy (viewportScroll ActivityList) 15
    Just _ -> vScrollBy (viewportScroll ActivityDetail) 15
handleActivitiesEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) = do
  s <- get
  case s ^. activitiesState . asExpanded of
    Nothing -> do
      -- Page up in list view
      modify $ activitiesState . asSelected %~ (\i -> max (i - 15) 0)
      vScrollBy (viewportScroll ActivityList) (-15)
    Just _ -> vScrollBy (viewportScroll ActivityDetail) (-15)
handleActivitiesEvent (V.EvKey (V.KChar 'l') []) = openDetail
handleActivitiesEvent (V.EvKey V.KEnter []) = openDetail
handleActivitiesEvent _ = pure ()

openDetail :: EventM Name AppState ()
openDetail = do
  s <- get
  let sel = s ^. activitiesState . asSelected
  modify $ activitiesState . asExpanded .~ Just sel

closeDetail :: EventM Name AppState ()
closeDetail = modify $ activitiesState . asExpanded .~ Nothing
