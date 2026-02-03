# Development Guide

## Prerequisites

- GHC 9.4+ (via ghcup or nix)
- PostgreSQL 15+
- Google Cloud project with Gmail and Calendar APIs enabled
- Anthropic API key

## Quick Start

```bash
# Create database
createdb wisp

# Copy example config
cp wisp.yaml.example wisp.yaml

# Set environment variables (or put in .envrc)
export GOOGLE_CLIENT_ID="your-client-id"
export GOOGLE_CLIENT_SECRET="your-client-secret"
export ANTHROPIC_API_KEY="your-api-key"

# Build and run server
cd wisp-srv
cabal build
cabal run wisp-srv

# In another terminal, run CLI
cd wisp-cli
cabal build
cabal run wisp-cli -- status
```

## Project Structure

```
wisp/
├── wisp-srv/          # Backend server (Haskell)
├── wisp-cli/          # Command line interface (Haskell)
└── docs/              # Documentation
```

## Development Workflow

### Running Tests

```bash
cd wisp-srv
cabal test
```

Tests use a transaction-rollback pattern - each test runs in a transaction that's rolled back, keeping the database clean.

### Adding a New Endpoint

1. Create handler in `src/Http/Handlers/`
2. Add route in `src/Http/Routes.hs`
3. Export from handler module

### Adding a Database Query

1. Add function in appropriate `src/Infra/Db/` module
2. Use `runQuery` or `runExecute` from App.Monad
3. Add corresponding Spec test

### Adding a New Service

1. Create module in `src/Services/`
2. Add to `other-modules` in cabal file
3. Wire up in `app/Main.hs` if it's a background service

## Code Patterns

### App Monad

All application code runs in the `App` monad:

```haskell
type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
```

Access environment via helpers:

```haskell
getDbPool :: App (Pool Connection)
getLogger :: App LoggerSet
getConfig :: App Config
```

### Database Queries

Use postgresql-simple with direct queries:

```haskell
getActivity :: EntityId -> App (Maybe Activity)
getActivity eid = do
  pool <- getDbPool
  results <- liftIO $ withResource pool $ \conn ->
    query conn "SELECT ... FROM activities WHERE id = ?" (Only (unEntityId eid))
  pure $ fmap unDbActivity $ listToMaybe results
```

The `DbActivity` newtype wraps `Activity` to provide the `FromRow` instance without orphans.

### Entity IDs

All database IDs use the `EntityId` newtype with NanoID generation:

```haskell
newtype EntityId = EntityId { unEntityId :: Text }

generateId :: IO EntityId
```

### Structured Logging

```haskell
logInfo msg = do
  lgr <- getLogger
  liftIO $ pushLogStrLn lgr $ toLogStr $ encodeUtf8 $ "[INFO] " <> msg
```

### STM Queues

The classification system uses STM for concurrent queue processing:

```haskell
-- Enqueue activities for classification
enqueueActivities :: TQueue EntityId -> [EntityId] -> IO ()

-- Workers block until work available
dequeueActivity :: TQueue EntityId -> IO EntityId
```

### 12-Factor Chat Actions

Chat returns structured JSON, server executes:

```haskell
-- LLM returns this structure
data ChatAction
  = MarkComplete ActionFilter
  | ApproveActivity Text
  | DismissActivity Text
  | NoAction Text

-- Server executes deterministically
executeAction :: ChatAction -> App ActionResult
```

This keeps control flow in application code, not the LLM.

## Configuration

### wisp.yaml

```yaml
server:
  host: "127.0.0.1"
  port: 8080

database:
  url: "postgres://localhost:5432/wisp"

google:
  clientId: "set-via-env"
  clientSecret: "set-via-env"

polling:
  intervalMinutes: 5

classification:
  confidenceThreshold: 0.5
  workerCount: null  # null = auto-detect CPUs

claude:
  apiKey: "set-via-env"
  model: "claude-sonnet-4-20250514"

notifications:
  enabled: true
  default_interval_hours: 4
  urgent_interval_hours: 2
  urgent_threshold_count: 3
  quiet_hours_start: "22:00"
  quiet_hours_end: "08:00"
  vip_emails: []
```

### Environment Variables

Secrets should be set via environment:

- `GOOGLE_CLIENT_ID`
- `GOOGLE_CLIENT_SECRET`
- `ANTHROPIC_API_KEY`
- `DATABASE_URL` (optional, overrides config)

## Database

### Migrations

Migrations are SQL files in `wisp-srv/migrations/`, run alphabetically:

```
001_accounts.sql
002_activities.sql
003_people.sql
...
```

Migrations run automatically on server startup.

### Schema Overview

- `accounts` - Google accounts with OAuth tokens
- `activities` - Emails/events with classification data
- `people` - Contacts linked to activities
- `receipts` - Processing audit log
- `notification_state` - Singleton tracking last notification
- `poll_state` - Per-account last poll timestamps

## Background Services

The server starts these concurrent threads:

1. **HTTP Server** - REST API (main thread)
2. **Polling Loop** - Fetches Gmail/Calendar on interval
3. **Classification Workers** - N workers processing queue
4. **Notification Loop** - Checks every 15 min for notifications

All threads are linked to main - if one crashes, all crash.

## Debugging

### Check Server Health

```bash
curl http://localhost:8080/health
```

### View Activity Stats

```bash
wisp status
```

### Trigger Manual Poll

```bash
wisp poll
# or
curl -X POST http://localhost:8080/poll
```

### View Processing Logs

```bash
curl http://localhost:8080/activities/ACTIVITY_ID/logs
```

## Common Issues

### "column X does not exist"

Migration hasn't run. Check migrations are in correct directory and numbered properly.

### OAuth token expired

Re-authenticate via:
```bash
curl http://localhost:8080/auth/google
# Follow the URL, complete OAuth flow
```

### Classification stuck

Check worker count and queue. Workers log their progress:
```
[Worker 1] Classified abc123 -> Surfaced
```

### Tests failing with database errors

Ensure test database exists and TEST_DATABASE_URL is set (or it will use the default wisp database).
