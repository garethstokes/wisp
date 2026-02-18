# Knowledge + Memory System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement shared knowledge (tagged notes) and per-agent memory (sessions, summaries, souls) with the new agents/skills architecture.

**Architecture:**
- **Skills** = prompts (in knowledge) + tools (in code) + context fetching
- **Agents** = named personas (in knowledge) with memory/soul, can activate skills
- Notes are Activities with `source: Note` and a `tags TEXT[]` column
- Sessions/summaries/souls keyed by agent name (not skill path)

**Tech Stack:** Haskell, PostgreSQL, existing App monad, Aeson for JSON

---

## Completed Phases

### Phase 1: Foundation (Tags + Notes) ✅

- Task 1: Database migration for tags column ✅
- Task 2: Update Activity domain model with tags ✅
- Task 3: Update Activity database layer ✅
- Task 4: Note insertion and tag operations ✅

### Phase 2: Memory (Sessions, Summaries, Souls) ✅

- Task 5: Database migration for memory tables ✅
- Task 6: Session domain model ✅
- Task 7: Summary domain model ✅
- Task 8: Soul domain model ✅
- Task 9: Session database layer ✅
- Task 10: Soul database layer ✅
- Task 11: Summary database layer ✅

---

## Phase 3: Skills Infrastructure

### Task 12: Rename Agents/ to Skills/

**Files:**
- Rename: `wisp-srv/src/Agents/Concierge.hs` → `wisp-srv/src/Skills/Concierge.hs`
- Rename: `wisp-srv/src/Agents/Concierge/` → `wisp-srv/src/Skills/Concierge/`
- Rename: `wisp-srv/src/Agents/Scheduler.hs` → `wisp-srv/src/Skills/Scheduler.hs`
- Rename: `wisp-srv/src/Agents/Insights.hs` → `wisp-srv/src/Skills/Insights.hs`
- Rename: `wisp-srv/src/Agents/Housekeeper.hs` → `wisp-srv/src/Skills/Housekeeper.hs`
- Keep: `wisp-srv/src/Agents/Run.hs` (will become agent core)
- Update: `wisp-srv/wisp-srv.cabal`

**Step 1: Create Skills directory and move files**

```bash
mkdir -p wisp-srv/src/Skills
mv wisp-srv/src/Agents/Concierge.hs wisp-srv/src/Skills/
mv wisp-srv/src/Agents/Concierge wisp-srv/src/Skills/
mv wisp-srv/src/Agents/Scheduler.hs wisp-srv/src/Skills/
mv wisp-srv/src/Agents/Insights.hs wisp-srv/src/Skills/
mv wisp-srv/src/Agents/Housekeeper.hs wisp-srv/src/Skills/
```

**Step 2: Update module declarations**

Change `module Agents.Concierge` to `module Skills.Concierge` in each file.
Update all imports across the codebase.

**Step 3: Update cabal file**

Replace `Agents.Concierge` etc with `Skills.Concierge` etc.

**Step 4: Verify build**

Run: `cd wisp-srv && cabal build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add -A
git commit -m "refactor: rename Agents/ to Skills/"
```

---

### Task 13: Skill Domain Model

**Files:**
- Create: `wisp-srv/src/Domain/Skill.hs`
- Create: `wisp-srv/test/Domain/SkillSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the test**

```haskell
module Domain.SkillSpec (spec) where

import Test.Hspec
import Domain.Skill

spec :: Spec
spec = describe "Skill" $ do
  it "parses skill name from tag" $ do
    parseSkillTag "skill:concierge" `shouldBe` Just "concierge"
    parseSkillTag "skill:scheduler" `shouldBe` Just "scheduler"
    parseSkillTag "other" `shouldBe` Nothing

  it "creates skill tag from name" $ do
    skillTag "concierge" `shouldBe` "skill:concierge"
```

**Step 2: Write Domain/Skill.hs**

```haskell
module Domain.Skill
  ( SkillName
  , skillTag
  , skillPromptTags
  , parseSkillTag
  ) where

import Data.Text (Text)
import qualified Data.Text as T

type SkillName = Text

-- Create the tag for a skill's prompt note
skillTag :: SkillName -> Text
skillTag name = "skill:" <> name

-- Tags that identify a skill's prompt note
skillPromptTags :: SkillName -> [Text]
skillPromptTags name = [skillTag name, "prompt"]

-- Parse skill name from a tag like "skill:concierge"
parseSkillTag :: Text -> Maybe SkillName
parseSkillTag t = case T.stripPrefix "skill:" t of
  Just name | not (T.null name) -> Just name
  _ -> Nothing
```

**Step 3: Update cabal and run tests**

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Skill.hs wisp-srv/test/Domain/SkillSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Skill types"
```

---

### Task 14: Agent Domain Model

**Files:**
- Create: `wisp-srv/src/Domain/Agent.hs` (replace existing AgentInfo)
- Create: `wisp-srv/test/Domain/AgentSpec.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Write the test**

```haskell
module Domain.AgentSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, encode)
import Domain.Agent

spec :: Spec
spec = describe "Agent" $ do
  it "parses agent name from tag" $ do
    parseAgentTag "agent:jarvis" `shouldBe` Just "jarvis"
    parseAgentTag "other" `shouldBe` Nothing

  it "round-trips AgentConfig through JSON" $ do
    let config = AgentConfig
          { agentPersonalitySeed = "Formal, concise"
          , agentActiveSkill = Just "concierge"
          }
    decode (encode config) `shouldBe` Just config
```

**Step 2: Write Domain/Agent.hs**

```haskell
module Domain.Agent
  ( AgentName
  , AgentConfig(..)
  , agentTag
  , parseAgentTag
  , emptyAgentConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

type AgentName = Text

data AgentConfig = AgentConfig
  { agentPersonalitySeed :: Text
  , agentActiveSkill :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON AgentConfig where
  toJSON c = object
    [ "personality_seed" .= agentPersonalitySeed c
    , "active_skill" .= agentActiveSkill c
    ]

instance FromJSON AgentConfig where
  parseJSON = withObject "AgentConfig" $ \v -> AgentConfig
    <$> v .: "personality_seed"
    <*> v .:? "active_skill"

emptyAgentConfig :: AgentConfig
emptyAgentConfig = AgentConfig
  { agentPersonalitySeed = ""
  , agentActiveSkill = Nothing
  }

-- Create the tag for an agent definition note
agentTag :: AgentName -> Text
agentTag name = "agent:" <> name

-- Parse agent name from a tag like "agent:jarvis"
parseAgentTag :: Text -> Maybe AgentName
parseAgentTag t = case T.stripPrefix "agent:" t of
  Just name | not (T.null name) -> Just name
  _ -> Nothing
```

**Step 3: Update cabal and run tests**

**Step 4: Commit**

```bash
git add wisp-srv/src/Domain/Agent.hs wisp-srv/test/Domain/AgentSpec.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(domain): add Agent types for named personas"
```

---

### Task 15: Skill Registry

**Files:**
- Create: `wisp-srv/src/Skills/Registry.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create the registry**

```haskell
module Skills.Registry
  ( Skill(..)
  , SkillContext(..)
  , getSkill
  , allSkillNames
  , conciergeSkill
  , schedulerSkill
  , insightsSkill
  ) where

import Data.Text (Text)
import Data.Aeson (Value)
import Domain.Activity (Activity)
import Domain.Person (Person)
import App.Monad (App)

-- Context that a skill can provide
data SkillContext = SkillContext
  { skillContextActivities :: [Activity]
  , skillContextPeople :: [Person]
  , skillContextExtra :: Value  -- Skill-specific data
  } deriving (Show)

-- A skill provides tools and can fetch its own context
data Skill = Skill
  { skillName :: Text
  , skillToolNames :: [Text]           -- Tool names this skill provides
  , skillFetchContext :: App SkillContext  -- How to get skill-specific context
  }

-- Registry of available skills
allSkillNames :: [Text]
allSkillNames = ["concierge", "scheduler", "insights"]

getSkill :: Text -> Maybe Skill
getSkill "concierge" = Just conciergeSkill
getSkill "scheduler" = Just schedulerSkill
getSkill "insights" = Just insightsSkill
getSkill _ = Nothing

-- Skill definitions (context fetching delegates to existing code)
conciergeSkill :: Skill
conciergeSkill = Skill
  { skillName = "concierge"
  , skillToolNames = ["classify_email", "archive_email", "search_emails"]
  , skillFetchContext = fetchConciergeContext
  }

schedulerSkill :: Skill
schedulerSkill = Skill
  { skillName = "scheduler"
  , skillToolNames = ["list_events", "create_event", "update_event"]
  , skillFetchContext = fetchSchedulerContext
  }

insightsSkill :: Skill
insightsSkill = Skill
  { skillName = "insights"
  , skillToolNames = ["analyze_activity", "generate_summary"]
  , skillFetchContext = fetchInsightsContext
  }

-- Context fetching (extract from existing agent code)
fetchConciergeContext :: App SkillContext
fetchConciergeContext = do
  -- TODO: Extract from Skills.Concierge
  pure $ SkillContext [] [] (toJSON ())

fetchSchedulerContext :: App SkillContext
fetchSchedulerContext = do
  -- TODO: Extract from Skills.Scheduler
  pure $ SkillContext [] [] (toJSON ())

fetchInsightsContext :: App SkillContext
fetchInsightsContext = do
  -- TODO: Extract from Skills.Insights
  pure $ SkillContext [] [] (toJSON ())
```

**Step 2: Update cabal**

**Step 3: Verify build**

**Step 4: Commit**

```bash
git add wisp-srv/src/Skills/Registry.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(skills): add skill registry"
```

---

### Task 16: Base Tools Module

**Files:**
- Create: `wisp-srv/src/Skills/Base.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create base tools**

```haskell
module Skills.Base
  ( baseToolNames
  , executeBaseTool
  , BaseToolCall(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), withObject, (.:))
import Data.Text (Text)
import Domain.Id (EntityId)
import App.Monad (App)
import Infra.Db.Activity (insertNote, getActivitiesByTags, getActivity)

-- Tools available to all agents (without skills)
baseToolNames :: [Text]
baseToolNames =
  [ "search_knowledge"
  , "read_note"
  , "add_note"
  , "activate_skill"
  ]

data BaseToolCall
  = SearchKnowledge [Text] Int          -- tags, limit
  | ReadNote EntityId                    -- note ID
  | AddNote Text [Text]                  -- content, tags
  | ActivateSkill Text                   -- skill name
  deriving (Show)

executeBaseTool :: EntityId -> BaseToolCall -> App (Either Text Value)
executeBaseTool accountId tool = case tool of
  SearchKnowledge tags limit -> do
    notes <- getActivitiesByTags accountId tags limit
    pure $ Right $ toJSON notes

  ReadNote noteId -> do
    mNote <- getActivity noteId
    case mNote of
      Just note -> pure $ Right $ toJSON note
      Nothing -> pure $ Left "Note not found"

  AddNote content tags -> do
    let rawMeta = object ["origin" .= ("agent" :: Text)]
    mId <- insertNote accountId content tags rawMeta
    case mId of
      Just aid -> pure $ Right $ object ["note_id" .= aid, "status" .= ("created" :: Text)]
      Nothing -> pure $ Left "Failed to create note"

  ActivateSkill skillName -> do
    -- Returns a request for user confirmation
    pure $ Right $ object
      [ "action" .= ("request_permission" :: Text)
      , "skill" .= skillName
      , "message" .= ("Activate " <> skillName <> " skill?")
      ]
```

**Step 2: Update cabal**

**Step 3: Commit**

```bash
git add wisp-srv/src/Skills/Base.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(skills): add base tools for all agents"
```

---

## Phase 4: Agent Core

### Task 17: Agent Core Module

**Files:**
- Create: `wisp-srv/src/Agents/Core.hs`
- Modify: `wisp-srv/wisp-srv.cabal`

**Step 1: Create agent core**

```haskell
module Agents.Core
  ( handleAgentChat
  , loadAgent
  , Agent(..)
  ) where

import Data.Aeson (Value, decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Agent (AgentName, AgentConfig(..), agentTag, emptyAgentConfig)
import Domain.Skill (SkillName, skillPromptTags)
import Domain.Soul (Soul(..))
import Domain.Chat (ChatMessage, ChatResponse)
import Domain.Activity (Activity(..), activityRaw)
import Skills.Registry (Skill(..), getSkill)
import Skills.Base (baseToolNames)
import Infra.Db.Activity (getActivitiesByTags)
import Infra.Db.Soul (getOrCreateSoul)
import App.Monad (App)

data Agent = Agent
  { agentName :: AgentName
  , agentConfig :: AgentConfig
  , agentSoul :: Soul
  , agentActiveSkill :: Maybe Skill
  }

-- Load an agent from knowledge
loadAgent :: AgentName -> App (Maybe Agent)
loadAgent name = do
  -- Find agent definition note
  let tags = [agentTag name]
  notes <- getActivitiesByTags (error "TODO: account") tags 1
  case notes of
    [] -> pure Nothing
    (note:_) -> do
      let config = case decode (encode (activityRaw note)) of
            Just c -> c
            Nothing -> emptyAgentConfig
      soul <- getOrCreateSoul name
      mSkill <- case agentActiveSkill config of
        Just skillName -> pure $ getSkill skillName
        Nothing -> pure Nothing
      pure $ Just Agent
        { agentName = name
        , agentConfig = config
        , agentSoul = soul
        , agentActiveSkill = mSkill
        }

-- Build system prompt for agent
buildSystemPrompt :: Agent -> Maybe Text -> Text
buildSystemPrompt agent mSkillPrompt = T.unlines
  [ "You are " <> agentName agent <> ", a personal assistant."
  , ""
  , "## Your Personality"
  , if T.null (agentPersonalitySeed (agentConfig agent))
    then "Be helpful and concise."
    else agentPersonalitySeed (agentConfig agent)
  , ""
  , buildSoulSection (agentSoul agent)
  , ""
  , case mSkillPrompt of
      Just prompt -> "## Active Skill\n\n" <> prompt
      Nothing -> "## Available Actions\n\nYou can search and read knowledge, add notes, or activate a skill for specialized tasks."
  , ""
  , "## Tools"
  , T.unlines $ map ("- " <>) $ baseToolNames <> maybe [] skillToolNames (agentActiveSkill agent)
  ]

buildSoulSection :: Soul -> Text
buildSoulSection soul
  | T.null (soulPersonality soul) && null (soulInsights soul) = ""
  | otherwise = T.unlines
      [ "## Insights About This User"
      , ""
      , if T.null (soulPersonality soul)
        then ""
        else "Communication style: " <> soulPersonality soul
      , ""
      , if null (soulInsights soul)
        then ""
        else T.unlines $ map ("- " <>) (soulInsights soul)
      ]

-- Handle chat for an agent
handleAgentChat :: AgentName -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
handleAgentChat agentName messages tz = do
  mAgent <- loadAgent agentName
  case mAgent of
    Nothing -> pure $ Left $ "Agent not found: " <> agentName
    Just agent -> do
      -- Load skill prompt if skill is active
      mSkillPrompt <- case agentActiveSkill agent of
        Nothing -> pure Nothing
        Just skill -> loadSkillPrompt (skillName skill)

      let systemPrompt = buildSystemPrompt agent mSkillPrompt

      -- TODO: Call LLM with assembled prompt
      -- TODO: Handle tool calls (base + skill tools)
      -- TODO: Update session

      pure $ Left "Not yet implemented"

-- Load skill prompt from knowledge
loadSkillPrompt :: SkillName -> App (Maybe Text)
loadSkillPrompt name = do
  let tags = skillPromptTags name
  notes <- getActivitiesByTags (error "TODO: account") tags 1
  case notes of
    [] -> pure Nothing
    (note:_) -> pure $ activityTitle note  -- Prompt stored in title for now
```

**Step 2: Update cabal**

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Core.hs wisp-srv/wisp-srv.cabal
git commit -m "feat(agents): add unified agent core"
```

---

### Task 18: Update Dispatcher

**Files:**
- Modify: `wisp-srv/src/Agents/Dispatcher.hs`

**Step 1: Update dispatcher to use agent core**

```haskell
module Agents.Dispatcher
  ( dispatchChat
  , listAgents
  ) where

import Data.Text (Text)
import Domain.Chat (ChatMessage, ChatResponse)
import Domain.Agent (agentTag)
import App.Monad (App)
import Agents.Core (handleAgentChat)
import Infra.Db.Activity (getActivitiesByTags)

-- Dispatch chat to an agent by name
dispatchChat :: Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat agentName messages tz = handleAgentChat agentName messages tz

-- List available agents (from knowledge)
listAgents :: App [Text]
listAgents = do
  -- Find all agent definition notes
  notes <- getActivitiesByTags (error "TODO: account") ["agent:*"] 100
  pure $ map extractAgentName notes
  where
    extractAgentName _ = "TODO"  -- Parse from tags
```

**Step 2: Update HTTP routes to use agent names**

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Dispatcher.hs
git commit -m "refactor(dispatcher): route to agents by name"
```

---

### Task 19: Seed Initial Agent and Skills

**Files:**
- Create: `wisp-srv/seeds/001_agents_and_skills.sql`

**Step 1: Create seed data**

```sql
-- Seed default agent
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags)
VALUES (
  'seed-agent-wisp',
  'default',  -- TODO: real account
  'note',
  'seed-agent-wisp',
  '{"personality_seed": "Helpful, concise, proactive", "active_skill": null}',
  'Wisp - Default Agent',
  'processed',
  ARRAY['agent:wisp']
);

-- Seed concierge skill prompt
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags)
VALUES (
  'seed-skill-concierge',
  'default',
  'note',
  'seed-skill-concierge',
  '{}',
  'You are a concierge helping manage email and communications.

## Tools

- classify_email: Classify an email by urgency and type
- archive_email: Archive an email
- search_emails: Search through emails

## Guidelines

- Be concise and action-oriented
- Prioritize urgent items
- Ask before taking irreversible actions',
  'processed',
  ARRAY['skill:concierge', 'prompt']
);

-- Seed scheduler skill prompt
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags)
VALUES (
  'seed-skill-scheduler',
  'default',
  'note',
  'seed-skill-scheduler',
  '{}',
  'You are a scheduler helping manage calendar and time.

## Tools

- list_events: List calendar events
- create_event: Create a new event
- update_event: Update an existing event

## Guidelines

- Consider time zones
- Check for conflicts before scheduling
- Respect buffer time between meetings',
  'processed',
  ARRAY['skill:scheduler', 'prompt']
);

-- Seed insights skill prompt
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags)
VALUES (
  'seed-skill-insights',
  'default',
  'note',
  'seed-skill-insights',
  '{}',
  'You are an analyst helping understand patterns and generate insights.

## Tools

- analyze_activity: Analyze activity patterns
- generate_summary: Generate a summary of recent activity

## Guidelines

- Look for patterns across time
- Surface actionable insights
- Be specific with recommendations',
  'processed',
  ARRAY['skill:insights', 'prompt']
);
```

**Step 2: Commit**

```bash
git add wisp-srv/seeds/
git commit -m "feat(seeds): add initial agent and skill prompts"
```

---

## Phase 5: Skill Activation Flow

### Task 20: Activate/Deactivate Skill Tools

**Files:**
- Modify: `wisp-srv/src/Skills/Base.hs`
- Modify: `wisp-srv/src/Agents/Core.hs`

**Step 1: Implement skill activation**

In `Skills/Base.hs`, add `deactivate_skill` to base tools when a skill is active.

In `Agents/Core.hs`, implement:
- `activateSkill :: AgentName -> SkillName -> App (Either Text ())`
- `deactivateSkill :: AgentName -> App (Either Text ())`

These update the agent's note in knowledge (`active_skill` field).

**Step 2: Add permission flow**

When `activate_skill` is called:
1. Return a pending state requesting user confirmation
2. On confirmation, update agent note
3. Reload agent with new skill active

**Step 3: Commit**

```bash
git add wisp-srv/src/Skills/Base.hs wisp-srv/src/Agents/Core.hs
git commit -m "feat(skills): implement activate/deactivate flow"
```

---

### Task 21: Wire Up Skill Tool Dispatch

**Files:**
- Modify: `wisp-srv/src/Agents/Core.hs`
- Modify: `wisp-srv/src/Skills/Concierge.hs`
- Modify: `wisp-srv/src/Skills/Scheduler.hs`
- Modify: `wisp-srv/src/Skills/Insights.hs`

**Step 1: Extract tool execution from skills**

Each skill module exports a `executeSkillTool` function:

```haskell
-- In Skills/Concierge.hs
executeConciergeTool :: EntityId -> Text -> Value -> App (Either Text Value)
executeConciergeTool accountId toolName params = case toolName of
  "classify_email" -> ...
  "archive_email" -> ...
  _ -> pure $ Left $ "Unknown tool: " <> toolName
```

**Step 2: Wire into agent core**

```haskell
-- In Agents/Core.hs
executeTool :: Agent -> EntityId -> Text -> Value -> App (Either Text Value)
executeTool agent accountId toolName params
  | toolName `elem` baseToolNames = executeBaseTool accountId (parseBaseTool toolName params)
  | otherwise = case agentActiveSkill agent of
      Nothing -> pure $ Left $ "No skill active for tool: " <> toolName
      Just skill -> executeSkillTool skill accountId toolName params
```

**Step 3: Commit**

```bash
git add wisp-srv/src/Agents/Core.hs wisp-srv/src/Skills/*.hs
git commit -m "feat(agents): wire up skill tool dispatch"
```

---

## Phase 6: HTTP API

### Task 22: Agents API Endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Agents.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Create handlers**

```haskell
-- GET /agents - list available agents
-- GET /agents/:name - get agent details
-- POST /agents/:name/chat - chat with agent
-- POST /agents/:name/activate/:skill - activate skill (with permission)
-- POST /agents/:name/deactivate - deactivate current skill
```

**Step 2: Add routes**

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Agents.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(http): add agents API endpoints"
```

---

### Task 23: Skills API Endpoints

**Files:**
- Create: `wisp-srv/src/Http/Handlers/Skills.hs`
- Modify: `wisp-srv/src/Http/Routes.hs`

**Step 1: Create handlers**

```haskell
-- GET /skills - list available skills
-- GET /skills/:name - get skill details (prompt, tools)
-- PUT /skills/:name - update skill prompt
```

**Step 2: Add routes**

**Step 3: Commit**

```bash
git add wisp-srv/src/Http/Handlers/Skills.hs wisp-srv/src/Http/Routes.hs
git commit -m "feat(http): add skills API endpoints"
```

---

### Task 24: Final Verification

**Step 1: Run full test suite**

Run: `cd wisp-srv && cabal test`
Expected: All tests PASS

**Step 2: Build the project**

Run: `cd wisp-srv && cabal build`
Expected: Build succeeds

**Step 3: Apply migrations and seeds**

**Step 4: Manual testing**

- Chat with default agent (no skill)
- Activate concierge skill
- Use concierge tools
- Deactivate skill

**Step 5: Final commit**

```bash
git add -A
git commit -m "chore: final cleanup for agents/skills architecture"
```

---

## Summary

| Phase | Tasks | What It Delivers |
|-------|-------|------------------|
| 1. Foundation | 1-4 ✅ | Tags column, Note source, tag operations |
| 2. Memory | 5-11 ✅ | Sessions, Summaries, Souls |
| 3. Skills | 12-16 | Rename to Skills/, Skill domain, Registry, Base tools |
| 4. Agent Core | 17-19 | Unified agent logic, dispatcher update, seed data |
| 5. Activation | 20-21 | Skill activate/deactivate, tool dispatch |
| 6. HTTP API | 22-24 | Agents/Skills endpoints, verification |

**Total: 24 tasks (11 complete, 13 remaining)**

After completing this plan:
- Skills are capability modules with prompts in knowledge
- Agents are named personas with memory and soul
- Agents start skill-less and can activate skills with permission
- Memory is keyed by agent name
- HTTP API for managing agents, skills, and chat
