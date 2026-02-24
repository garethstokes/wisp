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
--   Ctrl+D / PageDown: page down
--   Ctrl+U / PageUp: page up
--   j / Down arrow: scroll down one line
--   k / Up arrow: scroll up one line
--   Mouse scroll: scroll up/down
handleViewportScroll :: Name -> V.Event -> EventM Name s Bool
handleViewportScroll vpName = \case
  -- Page scrolling
  V.EvKey (V.KChar 'd') [V.MCtrl] -> do
    vScrollBy (viewportScroll vpName) pageSize
    pure True
  V.EvKey (V.KChar 'u') [V.MCtrl] -> do
    vScrollBy (viewportScroll vpName) (-pageSize)
    pure True
  V.EvKey V.KPageDown [] -> do
    vScrollBy (viewportScroll vpName) pageSize
    pure True
  V.EvKey V.KPageUp [] -> do
    vScrollBy (viewportScroll vpName) (-pageSize)
    pure True
  -- Line scrolling - vim keys
  V.EvKey (V.KChar 'j') [] -> do
    vScrollBy (viewportScroll vpName) 1
    pure True
  V.EvKey (V.KChar 'k') [] -> do
    vScrollBy (viewportScroll vpName) (-1)
    pure True
  -- Line scrolling - arrow keys
  V.EvKey V.KDown [] -> do
    vScrollBy (viewportScroll vpName) 1
    pure True
  V.EvKey V.KUp [] -> do
    vScrollBy (viewportScroll vpName) (-1)
    pure True
  -- Mouse scroll
  V.EvMouseDown _ _ V.BScrollDown _ -> do
    vScrollBy (viewportScroll vpName) 3
    pure True
  V.EvMouseDown _ _ V.BScrollUp _ -> do
    vScrollBy (viewportScroll vpName) (-3)
    pure True
  _ -> pure False
