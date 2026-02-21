module Tui.Views.Activities
  ( activitiesWidget
  , handleActivitiesEvent
  , ActivitiesAction(..)
  , handleActivitiesEventWithAction
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
import Wisp.Client (Activity(..), ActivitySource(..), ActivityStatus(..), ActivityMetrics(..), SourceCount(..))

-- | Activities view widget
activitiesWidget :: Maybe UTCTime -> ActivitiesState -> Widget Name
activitiesWidget mNow as = case as ^. asExpanded of
  Just idx | idx < length activities ->
    -- Show detail view for expanded activity
    detailView (activities !! idx)
  _ -> vBox
    [ metricsHeader (as ^. asMetrics)
    , filterBar as
    , activityList mNow as
    ]
  where
    activities = as ^. asActivities

-- | Display high-level metrics
metricsHeader :: Maybe ActivityMetrics -> Widget Name
metricsHeader Nothing = emptyWidget
metricsHeader (Just m) = vLimit 2 $ vBox
  [ hBox
      [ padRight (Pad 2) $ txt $ "Total: " <> T.pack (show (metricsTotal m))
      , padRight (Pad 2) $ txt $ "24h: " <> T.pack (show (metricsRecent m))
      , txt " │ "
      , hBox $ punctuate (txt " ") $ map renderSourceCount (metricsBySource m)
      ]
  , txt ""
  ]

renderSourceCount :: SourceCount -> Widget Name
renderSourceCount sc = hBox
  [ txt $ sourceIcon (sourceCountSource sc)
  , txt $ T.pack (show (sourceCountTotal sc))
  , txt "/"
  , txt $ T.pack (show (sourceCountRecent sc))
  ]

punctuate :: Widget Name -> [Widget Name] -> [Widget Name]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate sep (x:xs) = x : sep : punctuate sep xs

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
      hasMore = as ^. asHasMore
      isLoading = as ^. asLoading
      loadMoreWidget
        | isLoading = txt "  Loading..."
        | hasMore = txt "  [G] Load more activities"
        | otherwise = emptyWidget
  in if null activities
     then padAll 1 $ txt "No activities. Press 'r' to refresh."
     else viewport ActivityList Vertical $ vBox $
          zipWith (renderActivityRow mNow selected) [0..] activities
          ++ [loadMoreWidget]

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
sourceIcon UnknownSource = "❓"

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
sourceLabel UnknownSource = "Unknown"

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

-- | Actions that Activities view can request
data ActivitiesAction = NoAction | LoadMore
  deriving (Show, Eq)

-- | Handle event and return an action for the main app to handle
handleActivitiesEventWithAction :: V.Event -> EventM Name AppState ActivitiesAction
handleActivitiesEventWithAction (V.EvKey (V.KChar 'G') []) = do
  -- Load more when pressing G (vim-style go to end)
  s <- get
  let hasMore = s ^. activitiesState . asHasMore
      isLoading = s ^. activitiesState . asLoading
  if hasMore && not isLoading
    then do
      modify $ activitiesState . asLoading .~ True
      pure LoadMore
    else pure NoAction
handleActivitiesEventWithAction e = do
  handleActivitiesEvent e
  pure NoAction

openDetail :: EventM Name AppState ()
openDetail = do
  s <- get
  let sel = s ^. activitiesState . asSelected
  modify $ activitiesState . asExpanded .~ Just sel

closeDetail :: EventM Name AppState ()
closeDetail = modify $ activitiesState . asExpanded .~ Nothing
