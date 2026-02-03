# Design: Concierge Classify-Pending Flow

## Overview

Tie up the classify-pending deterministic flow by:
1. Documenting the classification contract in `docs/tools.md`
2. Adding receipts to the pipeline for audit trail
3. Restructuring modules to align with agent architecture

## 1. Classification Contract

Add to `docs/tools.md`:

```yaml
classify_activity:
  domain: storage_tools
  type: deterministic

  input:
    activity_id: Text
    content: Text        # email body or event description
    title: Text
    sender_email: Text?
    starts_at: DateTime?

  output:
    activity_type: Text      # meeting_request, newsletter, invoice, etc.
    urgency: Text            # low, normal, high
    autonomy_tier: Int       # 1-4
    confidence: Float        # 0.0-1.0
    personas: [Text]         # work, personal, finance, etc.
    reasoning: Text          # why this classification
    suggested_actions: [Text] # what could be done
    option_framing: Text?    # how to present if surfaced (nullable)
```

## 2. Receipts

Create receipts at each pipeline step:

| Step | action_taken | action_detail |
|------|--------------|---------------|
| Classification success | `classified` | reasoning |
| Classification failure | `classification_failed` | error message |
| Routing | `routed_to_{status}` | null |

## 3. Module Structure

### Files to move/rename

| Old | New |
|-----|-----|
| `src/Services/Pipeline.hs` | `src/Agents/Concierge.hs` |
| `src/Services/Classifier.hs` | `src/Agents/Concierge/Classifier.hs` |

### Final structure

```
src/
  Agents/
    Concierge.hs              # orchestration (classifyPending, classifyAllPending)
    Concierge/
      Classifier.hs           # LLM prompt + parsing for classification
  Services/
    Router.hs                 # shared - determines status from classification
    PeopleResolver.hs         # shared - resolves sender to person
    ...
  Infra/
    Claude/
      Client.hs               # shared - raw Claude API
```

### Export changes

`Agents.Concierge` exports:
- `classifyPending :: Activity -> App (Either Text ActivityStatus)` (was `processActivity`)
- `classifyAllPending :: Int -> App [(Text, Either Text ActivityStatus)]` (was `processPendingActivities`)

### Files to update

- `app/Main.hs` - import Agents.Concierge
- `Http/Handlers/Pipeline.hs` - import Agents.Concierge
- `wisp-srv.cabal` - update module list
- Test files - update imports

## 4. Domain Changes

Update `Domain.Classification` to add new fields:

```haskell
data Classification = Classification
  { classificationPersonas :: [Text]
  , classificationActivityType :: Text
  , classificationUrgency :: Text
  , classificationAutonomyTier :: Int
  , classificationConfidence :: Double
  , classificationSummary :: Maybe Text
  -- New fields:
  , classificationReasoning :: Text
  , classificationSuggestedActions :: [Text]
  , classificationOptionFraming :: Maybe Text
  }
```

Update the classifier prompt to request these fields.

## Implementation Order

1. Update `Domain.Classification` with new fields
2. Update `Agents.Concierge.Classifier` prompt and parsing
3. Add receipt creation to pipeline
4. Move/rename files to new structure
5. Update imports in dependent files
6. Update cabal module list
7. Run tests, fix any breakage
