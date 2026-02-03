# Design: Quarantine Interview Flow

## Overview

First decision flow for concierge agent. User requests help with quarantined items via chat, LLM presents options, user picks, LLM outputs tool calls.

## Flow

1. User: "help with quarantined item abc123" (or "review my quarantined items")
2. LLM queries items, identifies clusters if multiple
3. LLM presents 2-3 classification options with reasoning:
   ```
   This looks like it could be:
   A) Newsletter from marketing list → Tier 1 (auto-archive)
   B) Promo from vendor you've bought from → Tier 2 (note, don't surface)
   C) Important update from service you use → Tier 3 (review)
   ```
4. User: "B" (or "B for all of these")
5. LLM outputs tool call:
   ```json
   {
     "tool": "update_activities",
     "activity_ids": ["abc123"],
     "updates": {
       "status": "processed",
       "classification": { "autonomy_tier": 2, ... }
     }
   }
   ```
6. Dispatcher executes, returns result
7. LLM confirms to user

## Principles

- **Option framing**: Present choices, never instructions (per policy.md)
- **Batch capable**: LLM can cluster similar items and apply same classification
- **Context window**: Conversation history maintains state (Factor 3)
- **12-factor reducer**: LLM outputs structured tool calls, Haskell executes (Factor 4, 8)

## Tools

All tools accept arrays for batch operations.

### update_activities

```yaml
update_activities:
  input:
    activity_ids: [Text]
    updates:
      status: Text?              # pending, surfaced, quarantined, processed, archived
      classification:            # optional, partial updates allowed
        activity_type: Text?
        urgency: Text?
        autonomy_tier: Int?
        confidence: Float?
        personas: [Text]?
        reasoning: Text?
        suggested_actions: [Text]?
        option_framing: Text?
  output:
    updated_count: Int
    activity_ids: [Text]
```

### query_activities

```yaml
query_activities:
  input:
    status: Text?                # filter by status
    limit: Int?                  # default 20
    since: DateTime?             # created after
  output:
    activities: [Activity]
    count: Int
```

### query_people

```yaml
query_people:
  input:
    email: Text?                 # exact match
    search: Text?                # partial match on email or name
    limit: Int?
  output:
    people: [Person]
    count: Int
```

## Types

In `Agents.Concierge`:

```haskell
-- Tool calls concierge can output
data ConciergeToolCall
  = UpdateActivities [Text] ActivityUpdates
  | QueryActivities ActivityFilter
  | QueryPeople PeopleFilter
  deriving (Show, Eq)

data ActivityUpdates = ActivityUpdates
  { updatesStatus :: Maybe ActivityStatus
  , updatesClassification :: Maybe PartialClassification
  } deriving (Show, Eq)

data ActivityFilter = ActivityFilter
  { filterStatus :: Maybe ActivityStatus
  , filterLimit :: Maybe Int
  , filterSince :: Maybe UTCTime
  } deriving (Show, Eq)

data PeopleFilter = PeopleFilter
  { peopleEmail :: Maybe Text
  , peopleSearch :: Maybe Text
  , peopleLimit :: Maybe Int
  } deriving (Show, Eq)

-- Result from dispatcher
data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)
```

## Module Structure

All in `Agents.Concierge` for now:

```haskell
module Agents.Concierge
  ( -- Deterministic flows
    classifyPending
  , classifyAllPending
    -- Decision flows
  , handleChat
    -- Types
  , ConciergeToolCall(..)
  , ActivityUpdates(..)
  , ActivityFilter(..)
  , PeopleFilter(..)
  , ToolResult(..)
    -- Dispatcher
  , executeToolCall
  ) where
```

## Changes from Current Code

- Replace `Domain.ChatAction` with `ConciergeToolCall`
- Replace `Services.Chat` with chat handling in `Agents.Concierge`
- Update chat prompt to output tool calls as JSON
- Add dispatcher that pattern-matches on `ConciergeToolCall`

## Implementation Order

1. Define `ConciergeToolCall` and related types in `Agents.Concierge`
2. Implement `executeToolCall` dispatcher
3. Update chat prompt for option-framing and tool call output
4. Parse LLM response into `Maybe ConciergeToolCall`
5. Wire up: chat response either has tool call (execute) or not (display to user)
6. Remove `Domain.ChatAction` and `Services.Chat`
7. Test with quarantined items
