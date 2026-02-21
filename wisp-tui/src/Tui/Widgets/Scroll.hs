module Tui.Widgets.Scroll
  ( handleViewportScroll
  , pageSize
  ) where

import Brick
import qualified Graphics.Vty as V

import Tui.Types (Name)

-- | Number of lines to scroll on page up/down
pageSize :: Int
pageSize = 15

-- | Shared scrolling handler for any viewport.
-- Returns True if the event was handled, False otherwise.
-- Handles:
--   Ctrl+D: page down
--   Ctrl+U: page up
--   j: scroll down one line
--   k: scroll up one line
handleViewportScroll :: Name -> V.Event -> EventM Name s Bool
handleViewportScroll vpName = \case
  V.EvKey (V.KChar 'd') [V.MCtrl] -> do
    vScrollBy (viewportScroll vpName) pageSize
    pure True
  V.EvKey (V.KChar 'u') [V.MCtrl] -> do
    vScrollBy (viewportScroll vpName) (-pageSize)
    pure True
  V.EvKey (V.KChar 'j') [] -> do
    vScrollBy (viewportScroll vpName) 1
    pure True
  V.EvKey (V.KChar 'k') [] -> do
    vScrollBy (viewportScroll vpName) (-1)
    pure True
  _ -> pure False
