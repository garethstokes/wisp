# wisp-srv

Backend server for Wisp, the autonomy-preserving personal assistant.

## Prerequisites

- GHC 9.6+
- PostgreSQL 14+
- Cabal 3.8+

## Setup

1. Create database:
   ```bash
   createdb wisp
   createdb wisp_test  # for tests
   ```

2. Copy config:
   ```bash
   cp wisp.yaml.example wisp.yaml
   # Edit wisp.yaml with your settings
   ```

3. Set up Google OAuth:
   - Go to [Google Cloud Console](https://console.cloud.google.com/)
   - Create OAuth 2.0 credentials
   - Add `http://127.0.0.1:8080/auth/google/callback` as redirect URI
   - Update `wisp.yaml` with client ID and secret

4. Build and run:
   ```bash
   cabal build wisp-srv
   cabal run wisp-srv
   ```

## Development

Run tests:
```bash
cabal test
```

Build everything:
```bash
cabal build all
```

## API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check |
| `/auth/google` | GET | Start OAuth flow |
| `/auth/google/callback` | GET | OAuth callback |

## Architecture

The server uses Simple Haskell style:

- **App monad**: `ReaderT Env IO` for all effectful code
- **Persistence**: Versioned JSON entities in PostgreSQL
- **Job queue**: PostgreSQL-backed with `FOR UPDATE SKIP LOCKED`
- **HTTP**: Scotty for routing

See `docs/plans/2026-02-01-technical-design.md` for full architecture documentation.

## Project Structure

```
wisp-srv/
  app/
    Main.hs              # Entry point
  src/
    App/
      Config.hs          # Configuration types
      Env.hs             # Environment builder
      Monad.hs           # App monad definition
    Domain/
      Id.hs              # EntityId type
      Schema.hs          # Schema versioning
    Http/
      Server.hs          # HTTP server setup
      Routes.hs          # Route definitions
      Handlers/
        Health.hs        # Health endpoint
        Auth.hs          # OAuth endpoints
    Infra/
      Db/
        Auth.hs          # Token storage
        Entity.hs        # Entity CRUD
        Jobs.hs          # Job queue
        Migrations.hs    # Migration runner
      Google/
        Auth.hs          # OAuth client
  migrations/
    001_entities.sql
    002_jobs.sql
    003_receipts.sql
    004_auth.sql
    005_poll_state.sql
  test/
    ...                  # Test specs
```
