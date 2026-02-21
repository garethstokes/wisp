module Tui.Views.Skills
  ( skillsWidget
  , handleSkillsEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll)
import Wisp.Client (Skill(..))

-- | Skills view widget
skillsWidget :: SkillsState -> Widget Name
skillsWidget ss = case ss ^. ssExpanded of
  Nothing -> listView ss
  Just idx -> detailView ss idx

listView :: SkillsState -> Widget Name
listView ss = vBox
  [ padBottom (Pad 1) $ txt "  Skill                           Tools"
  , txt "  ------"
  , viewport SkillsList Vertical $ vBox $
      if null (ss ^. ssSkills)
        then [padAll 1 $ txt "No skills."]
        else zipWith (renderSkill (ss ^. ssSelected)) [0..] (ss ^. ssSkills)
  ]

renderSkill :: Int -> Int -> Skill -> Widget Name
renderSkill selected idx skill =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      name = skillName skill
      toolCount = length (skillTools skill)
      availTag = if skillAvailable skill then "" else " [unavailable]"
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 30 ' ' name
    , txt $ T.pack (show toolCount) <> " tools" <> availTag
    ]

detailView :: SkillsState -> Int -> Widget Name
detailView ss idx =
  let skills = ss ^. ssSkills
      mSkill = if idx < length skills
               then Just (skills !! idx)
               else Nothing
  in case mSkill of
    Nothing -> txt "Skill not found"
    Just skill -> vBox
      [ txt "[Esc/h to return]"
      , txt ""
      , viewport SkillsDetail Vertical $ skillDetailWidget skill
      ]

skillDetailWidget :: Skill -> Widget Name
skillDetailWidget skill = vBox
  [ txt $ "Skill: " <> skillName skill
  , txt $ "Available: " <> if skillAvailable skill then "Yes" else "No"
  , txt ""
  , txt "Tools:"
  , txt "------"
  , vBox $ if null (skillTools skill)
           then [txt "  (no tools)"]
           else map renderTool (skillTools skill)
  ]

renderTool :: Text -> Widget Name
renderTool name = txt $ "  - " <> name

-- | Handle skills-specific events
handleSkillsEvent :: V.Event -> EventM Name AppState ()
handleSkillsEvent evt = do
  s <- get
  case s ^. skillsState . ssExpanded of
    Just _ -> handleDetailEvent evt
    Nothing -> handleListEvent evt

handleListEvent :: V.Event -> EventM Name AppState ()
handleListEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (s ^. skillsState . ssSkills) - 1
  modify $ skillsState . ssSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
handleListEvent (V.EvKey (V.KChar 'k') []) =
  modify $ skillsState . ssSelected %~ (\i -> max 0 (i - 1))
handleListEvent (V.EvKey V.KEnter []) = do
  s <- get
  let idx = s ^. skillsState . ssSelected
  modify $ skillsState . ssExpanded .~ Just idx
handleListEvent (V.EvKey (V.KChar 'l') []) = do
  s <- get
  let idx = s ^. skillsState . ssSelected
  modify $ skillsState . ssExpanded .~ Just idx
handleListEvent evt = do
  handled <- handleViewportScroll SkillsList evt
  if handled then pure () else pure ()

handleDetailEvent :: V.Event -> EventM Name AppState ()
handleDetailEvent (V.EvKey V.KEsc []) =
  modify $ skillsState . ssExpanded .~ Nothing
handleDetailEvent (V.EvKey (V.KChar 'h') []) =
  modify $ skillsState . ssExpanded .~ Nothing
handleDetailEvent evt = do
  handled <- handleViewportScroll SkillsDetail evt
  if handled then pure () else pure ()
