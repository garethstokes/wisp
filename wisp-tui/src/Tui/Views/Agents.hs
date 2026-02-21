module Tui.Views.Agents
  ( agentsWidget
  , handleAgentsEvent
  ) where

import Brick
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (.~), (%~))

import Tui.Types
import Tui.Widgets.Scroll (handleViewportScroll)
import Wisp.Client (AgentInfo(..), AgentSoul(..), SessionSummary(..))

-- | Agents view widget
agentsWidget :: AgentsState -> Widget Name
agentsWidget as = case as ^. agsExpanded of
  Nothing -> listView as
  Just idx -> detailView as idx

listView :: AgentsState -> Widget Name
listView as = vBox
  [ padBottom (Pad 1) $ txt "  Agent                           Active Skill"
  , txt "  ------"
  , viewport AgentsList Vertical $ vBox $
      if null (as ^. agsAgents)
        then [padAll 1 $ txt "No agents."]
        else zipWith (renderAgent (as ^. agsSelected)) [0..] (as ^. agsAgents)
  ]

renderAgent :: Int -> Int -> AgentInfo -> Widget Name
renderAgent selected idx agent =
  let isSelected = idx == selected
      marker = if isSelected then "> " else "  "
      name = agentName agent
      skill = fromMaybe "(none)" (agentActiveSkill agent)
  in hBox
    [ txt marker
    , txt $ T.justifyLeft 30 ' ' name
    , txt skill
    ]

detailView :: AgentsState -> Int -> Widget Name
detailView as idx =
  let agents = as ^. agsAgents
      mAgent = if idx < length agents
               then Just (agents !! idx)
               else Nothing
  in case mAgent of
    Nothing -> txt "Agent not found"
    Just agent -> vBox
      [ txt "[Esc/h to return]"
      , txt ""
      , viewport AgentsDetail Vertical $ agentDetailWidget agent (as ^. agsSessions)
      ]

agentDetailWidget :: AgentInfo -> [SessionSummary] -> Widget Name
agentDetailWidget agent sessions = vBox
  [ txt $ "Agent: " <> agentName agent
  , txt $ "Active Skill: " <> fromMaybe "(none)" (agentActiveSkill agent)
  , txt ""
  , txt "Personality:"
  , txt "------------"
  , txtWrap $ agentPersonality agent
  , txt ""
  , soulSection (agentSoul agent)
  , txt ""
  , sessionsSection sessions
  , txt ""
  , txt "Available Skills:"
  , txt "-----------------"
  , vBox $ map (\s -> txt $ "  - " <> s) (agentAvailableSkills agent)
  ]

soulSection :: AgentSoul -> Widget Name
soulSection soul = vBox
  [ txt "Soul:"
  , txt "-----"
  , if T.null (soulPersonality soul)
    then txt "  (no evolved personality yet)"
    else txtWrap $ "  " <> soulPersonality soul
  , txt ""
  , txt "Insights:"
  , if null (soulInsights soul)
    then txt "  (no insights yet)"
    else vBox $ map (\i -> txtWrap $ "  - " <> i) (soulInsights soul)
  ]

sessionsSection :: [SessionSummary] -> Widget Name
sessionsSection sessions = vBox
  [ txt "Recent Sessions:"
  , txt "----------------"
  , if null sessions
    then txt "  (no sessions)"
    else vBox $ map renderSession sessions
  ]

renderSession :: SessionSummary -> Widget Name
renderSession sess =
  let timeStr = T.pack $ take 16 $ show (sessionCreatedAt sess)
      status = case sessionEndedAt sess of
        Just _ -> "[ended]"
        Nothing -> "[active]"
  in txt $ "  " <> timeStr <> " " <> status <> " (" <> T.pack (show $ sessionMessageCount sess) <> " msgs)"

-- | Handle agents-specific events
handleAgentsEvent :: V.Event -> EventM Name AppState ()
handleAgentsEvent evt = do
  s <- get
  case s ^. agentsState . agsExpanded of
    Just _ -> handleDetailEvent evt
    Nothing -> handleListEvent evt

handleListEvent :: V.Event -> EventM Name AppState ()
handleListEvent (V.EvKey (V.KChar 'j') []) = do
  s <- get
  let maxIdx = length (s ^. agentsState . agsAgents) - 1
  modify $ agentsState . agsSelected %~ (\i -> min (i + 1) (max 0 maxIdx))
handleListEvent (V.EvKey (V.KChar 'k') []) =
  modify $ agentsState . agsSelected %~ (\i -> max 0 (i - 1))
handleListEvent (V.EvKey V.KEnter []) = do
  s <- get
  let idx = s ^. agentsState . agsSelected
  modify $ agentsState . agsExpanded .~ Just idx
handleListEvent (V.EvKey (V.KChar 'l') []) = do
  s <- get
  let idx = s ^. agentsState . agsSelected
  modify $ agentsState . agsExpanded .~ Just idx
handleListEvent evt = do
  handled <- handleViewportScroll AgentsList evt
  if handled then pure () else pure ()

handleDetailEvent :: V.Event -> EventM Name AppState ()
handleDetailEvent (V.EvKey V.KEsc []) =
  modify $ agentsState . agsExpanded .~ Nothing
handleDetailEvent (V.EvKey (V.KChar 'h') []) =
  modify $ agentsState . agsExpanded .~ Nothing
handleDetailEvent evt = do
  handled <- handleViewportScroll AgentsDetail evt
  if handled then pure () else pure ()
