# GitHub Integration Design

## Overview

Add GitHub account integration to poll and store GitHub events (commits, PRs, issues, etc.) as activities.

## Goals

- Add GitHub OAuth to connect GitHub accounts
- Poll `/users/{username}/events` API every hour
- Store all GitHub event types as activities
- Use ETag-based caching to minimize API calls
- Backfill up to 300 events on first connection
- Refactor Account model to support multiple providers

## Account Model Refactor

### Current State

```haskell
data Account = Account
  { accountId :: EntityId
  , accountEmail :: Text
  , accountDisplayName :: Maybe Text
  , accountCreatedAt :: UTCTime
  }
```

### New State

```haskell
data AccountProvider = Google | GitHub | GitLab
  deriving (Eq, Show)

data Account = Account
  { accountId :: EntityId
  , accountProvider :: AccountProvider
  , accountDisplayName :: Maybe Text
  , accountDetails :: Value  -- JSON: provider-specific data
  , accountCreatedAt :: UTCTime
  }
```

### Provider-Specific Details (JSON)

**Google:**
```json
{ "email": "user@gmail.com" }
```
Note: Refresh tokens remain in `auth_tokens` table.

**GitHub:**
```json
{
  "username": "garethstokes",
  "access_token": "gho_xxxx"
}
```
GitHub OAuth tokens don't expire, stored directly in details.

## GitHub OAuth Flow

### Config

```
GITHUB_CLIENT_ID=xxx
GITHUB_CLIENT_SECRET=xxx
```

### Endpoints

- `GET /auth/github` - Redirect to GitHub authorization
- `GET /auth/github/callback` - Handle OAuth callback

### Flow

1. User visits `/auth/github`
2. Redirect to `https://github.com/login/oauth/authorize?client_id=xxx&scope=read:user`
3. User authorizes, GitHub redirects to callback with `code`
4. Exchange code for access token via `POST https://github.com/login/oauth/access_token`
5. Fetch user info from `GET https://api.github.com/user`
6. Create Account with provider=GitHub, store username and token in details

### Scopes

- `read:user` - Required to fetch user profile
- Events API is public, no additional scopes needed

## GitHub Events API

### Endpoint

```
GET https://api.github.com/users/{username}/events
```

### Response

Returns up to 30 events per page, 10 pages max (300 events total).

```json
[
  {
    "id": "12345678901",
    "type": "PushEvent",
    "actor": { "login": "garethstokes" },
    "repo": { "name": "org/repo" },
    "payload": {
      "commits": [
        { "sha": "abc123", "message": "feat: add feature" }
      ]
    },
    "created_at": "2025-02-19T10:00:00Z"
  }
]
```

### ETag Handling

- Send `If-None-Match: {etag}` header if ETag stored
- If `304 Not Modified`: no new events, skip processing
- Otherwise: extract `ETag` header, process events, store new ETag

### Rate Limits

- 5000 requests/hour for authenticated users
- Hourly polling uses 1-10 requests (pagination)
- Log `X-RateLimit-Remaining` for monitoring

## Activity Integration

### Extend ActivitySource

```haskell
data ActivitySource = Email | Calendar | Conversation | Note | GitHubEvent
```

### Activity Mapping

| Activity Field | GitHub Event Data |
|----------------|-------------------|
| `source` | `GitHubEvent` |
| `source_id` | `event.id` |
| `title` | "PushEvent to org/repo" |
| `raw` | Full event JSON |
| `sender_email` | `None` |
| `starts_at` | `event.created_at` |

### Commit Data in PushEvent

The `raw` JSON for PushEvents contains commit details:

```json
{
  "type": "PushEvent",
  "repo": { "name": "org/repo" },
  "payload": {
    "commits": [
      { "sha": "abc123", "message": "feat: add feature", "author": {...} }
    ]
  }
}
```

Agents can parse `raw` to extract individual commits when needed.

## Polling Strategy

### Initial Backfill

On first poll (no ETag stored):
1. Fetch all pages (up to 10 pages, 300 events)
2. Insert all events as activities
3. Store ETag from first page

### Subsequent Polls

1. Send request with stored ETag
2. If 304: nothing new, done
3. Otherwise: process new events, update ETag

### Deduplication

Check `source_id` exists before inserting (same as Gmail poller).

## Database Migrations

### Migration: Account Providers

```sql
-- Add provider column
ALTER TABLE accounts ADD COLUMN provider TEXT NOT NULL DEFAULT 'google';

-- Add details JSON column
ALTER TABLE accounts ADD COLUMN details JSONB NOT NULL DEFAULT '{}';

-- Migrate existing email to details
UPDATE accounts SET details = jsonb_build_object('email', email);

-- Drop email column
ALTER TABLE accounts DROP COLUMN email;
```

### Activity Source

If using enum type:
```sql
ALTER TYPE activity_source ADD VALUE 'github_event';
```

## File Structure

### New Files

```
wisp-srv/src/Infra/GitHub/
├── Auth.hs         # OAuth flow
└── Events.hs       # Events API client

wisp-srv/src/Services/
└── GitHubPoller.hs # Polling logic

wisp-srv/migrations/
└── 016_account_providers.sql

wisp-srv/test/Infra/GitHub/
├── AuthSpec.hs
└── EventsSpec.hs

wisp-srv/test/Services/
└── GitHubPollerSpec.hs
```

### Modified Files

- `Domain/Account.hs` - Add AccountProvider, change to JSON details
- `Domain/Activity.hs` - Add GitHubEvent to ActivitySource
- `Infra/Db/Account.hs` - Handle provider + JSON details
- `Http/Handlers/Auth.hs` - Add GitHub OAuth endpoints
- `Http/Routes.hs` - Wire up /auth/github routes
- `Services/Scheduler.hs` - Add hourly GitHub polling

## Implementation Order

1. Migration: Account refactor
2. Domain changes (Account, ActivitySource)
3. Infra/Db/Account.hs updates
4. Infra/GitHub/Auth.hs
5. Http/Handlers/Auth.hs (GitHub routes)
6. Infra/GitHub/Events.hs
7. Services/GitHubPoller.hs
8. Services/Scheduler.hs integration
9. Tests
