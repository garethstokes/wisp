module Tui.Widgets.Layout
  ( headerWidget
  , statusBarWidget
  , viewTabName
  ) where

import Brick
import Data.Text (Text)

import Tui.Types

-- | View tab display names
viewTabName :: View -> Text
viewTabName ChatView = "Chat"
viewTabName ActivitiesView = "Activities"
viewTabName DocumentsView = "Documents"
viewTabName ApprovalsView = "Approvals"

-- | Header with tabs
headerWidget :: View -> Text -> Widget Name
headerWidget current agent = vLimit 1 $ hBox
  [ tabsWidget current
  , fill ' '
  , txt agent
  , txt " "
  ]

tabsWidget :: View -> Widget Name
tabsWidget current = hBox $ map (renderTab current) [minBound..maxBound]
  where
    renderTab cur v
      | v == cur  = withAttr selectedTabAttr $ txt $ " [" <> viewTabName v <> "] "
      | otherwise = txt $ "  " <> viewTabName v <> "  "

selectedTabAttr :: AttrName
selectedTabAttr = attrName "selectedTab"

-- | Status bar
statusBarWidget :: Maybe Text -> Widget Name
statusBarWidget mStatus = vLimit 1 $ hBox
  [ withAttr connectedAttr $ txt " Connected"
  , txt " | "
  , case mStatus of
      Just msg -> txt msg
      Nothing -> txt "Tab:switch views  ?:help  q:quit"
  ]

connectedAttr :: AttrName
connectedAttr = attrName "connected"
