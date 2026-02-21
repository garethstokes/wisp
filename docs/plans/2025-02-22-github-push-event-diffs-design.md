# GitHub PushEvent Diffs Design

## Overview

Enrich GitHub PushEvent activities with full commit diffs by fetching diff data from GitHub's commit API during polling.

## Data Model

The existing `raw` JSONB field on activities stores the GitHub event payload. We enrich PushEvents by adding a `commits_with_diffs` array:

```json
{
  "type": "PushEvent",
  "repo": { "name": "garethstokes/wisp" },
  "payload": { "commits": [...] },

  "commits_with_diffs": [
    {
      "sha": "abc123def456...",
      "message": "feat: add user authentication",
      "author": "garethstokes",
      "url": "https://github.com/garethstokes/wisp/commit/abc123...",
      "diff": "diff --git a/src/Auth.hs b/src/Auth.hs\nnew file mode 100644\n..."
    }
  ]
}
```

Diffs are fetched via `GET /repos/{owner}/{repo}/commits/{sha}` with `Accept: application/vnd.github.diff` header.

## Implementation Flow

### New Events (During Polling)

1. `GitHubPoller.hs` fetches events via `/users/{username}/events`
2. For each PushEvent, before creating the activity:
   - Extract repo name and commits array from payload
   - For each commit SHA, fetch diff via GitHub commit API
   - Build `commits_with_diffs` array with SHA, message, author, URL, and diff text
   - Inject into raw payload JSON
3. Store enriched activity as normal

### Backfill (Existing Events)

Function `backfillPushEventDiffs`:
1. Query activities where `source = 'github_event'` and title starts with `'PushEvent'`
2. Filter to those without `commits_with_diffs` in raw JSON
3. For each: parse raw payload, fetch diffs, update raw field
4. Triggered via `POST /admin/backfill-github-diffs`

## New Module

`Infra/GitHub/Commits.hs`:

```haskell
-- Fetch the diff for a single commit
fetchCommitDiff
  :: Text  -- owner
  -> Text  -- repo
  -> Text  -- SHA
  -> Text  -- access token
  -> IO (Either Text Text)  -- diff text or error
```

## Error Handling

### API Failures

Store activity with error marker if diff fetch fails:

```json
{
  "sha": "abc123...",
  "message": "feat: add auth",
  "author": "garethstokes",
  "diff": null,
  "diff_error": "404: Commit not found (may have been force-pushed)"
}
```

Partial failures don't block the whole event from being stored.

### Rate Limiting

- GitHub allows 5000 requests/hour for authenticated users
- Log warning if rate limited, skip remaining diffs for that poll cycle
- Backfill includes small delay between requests

### Large Pushes

- GitHub Events API caps commits array at 20 per PushEvent
- Fetch diffs for all available commits, note if truncated
- No size limit on individual diffs (store everything)

## Files to Modify

**New:**
- `wisp-srv/src/Infra/GitHub/Commits.hs` - `fetchCommitDiff` function

**Modified:**
- `wisp-srv/src/Services/GitHubPoller.hs` - Enrich PushEvents with diffs
- `wisp-srv/src/Http/Routes.hs` - Add backfill endpoint
- `wisp-srv/src/Http/Handlers/Activities.hs` - Add backfill handler

**No changes needed:**
- Database schema (using existing `raw` JSONB)
- TUI (displays `activityRaw` which will include diffs)
- Client types

## Testing

- Trigger poll, verify PushEvents have `commits_with_diffs` in raw JSON
- Test backfill on existing events
- Verify TUI detail view shows diff content
