# Timezone Display Design

## Overview

Display dates in the user's local timezone for CLI output and agent context, while keeping the API and database in UTC.

## Key Decisions

- **Config location**: `~/.config/wisp/cli.yaml` with `timezone` field
- **Conversion point**: CLI and agent receive pre-converted local times
- **API unchanged**: REST endpoints continue returning UTC
- **Format**: Agent decides how to present dates to humans
- **Missing config**: Error and prompt user to set it (explicit over implicit)

## Architecture

```
Database (UTC) → Server API (UTC) → CLI reads config → Converts to local → Displays
                                  ↘ Chat API receives timezone param
                                    → Agent context uses local times
```

## 1. CLI Configuration

**File**: `~/.config/wisp/cli.yaml`

```yaml
timezone: "Europe/London"
```

**Behavior**:
- CLI reads this file on startup
- If file missing → error: "CLI config not found. Create ~/.config/wisp/cli.yaml with timezone setting"
- If timezone field missing → error: "timezone not set in ~/.config/wisp/cli.yaml"
- If timezone invalid → error: "Invalid timezone 'Foo/Bar'. Use IANA format like 'Europe/London'"

**Implementation**: New module `wisp-cli/src/Config.hs`

```haskell
data CliConfig = CliConfig
  { timezone :: Text  -- IANA timezone name
  }
```

## 2. Timezone Conversion Utility

**New module**: `wisp-cli/src/Time.hs`

**Dependencies**: Add `tz` package for proper IANA timezone support with DST handling.

**Core functions**:
```haskell
-- Load timezone from IANA name, fail if invalid
loadTimezone :: Text -> IO TZ

-- Convert UTC to local time in configured timezone
utcToLocal :: TZ -> UTCTime -> LocalTime
```

**Why `tz` package**: Haskell's `Data.Time.TimeZone` is a fixed offset. IANA zones like "Europe/London" have DST rules (UTC+0 in winter, UTC+1 in summer). The `tz` package loads the system timezone database.

## 3. CLI Integration

**Pattern**: Conversion at display time, after receiving API responses.

```haskell
main = do
  cfg <- loadCliConfig  -- errors if missing/invalid
  tz <- loadTimezone (timezone cfg)
  runCommand tz cmd

-- Display functions receive timezone
showActivity :: TZ -> Activity -> IO ()
showActivity tz activity = do
  let localStart = utcToLocal tz <$> activityStartsAt activity
  -- Format and print with local time
```

**Affected commands**:
- `wisp inbox` - activity timestamps
- `wisp review` - calendar event start times
- `wisp status` - last poll time

## 4. Chat API Change

**Endpoint**: `POST /chat`

**Request body**:
```json
{
  "message": "what's on today?",
  "agent": "concierge",
  "timezone": "Europe/London"
}
```

**Server behavior**:
- If `timezone` provided → convert timestamps to local before building agent context
- If `timezone` missing → use UTC (backwards compatible)

**Affected files**:
- `wisp-srv/src/Http/Handlers/Chat.hs` - parse timezone from request
- `wisp-srv/src/Agents/Concierge.hs` - convert times in `activityToJson`
- `wisp-cli/app/Main.hs` - include timezone in chat requests

## 5. Agent Context Format

**What the LLM sees** (after conversion):
```json
{
  "activities": [
    {
      "title": "Dentist appointment",
      "starts_at": "2026-02-04T14:30:00",
      "type": "calendar"
    }
  ]
}
```

No `Z` suffix or offset - just local time. The agent treats it as "the user's time" and formats naturally.

## Implementation Files

### New files
- `wisp-cli/src/Config.hs` - CLI config loading
- `wisp-cli/src/Time.hs` - timezone utilities

### Modified files
- `wisp-cli/wisp-cli.cabal` - add `tz` dependency, new modules
- `wisp-cli/app/Main.hs` - load config, pass timezone to commands and chat
- `wisp-srv/src/Http/Handlers/Chat.hs` - accept timezone parameter
- `wisp-srv/src/Agents/Concierge.hs` - convert times in activity context
- `wisp-srv/wisp-srv.cabal` - add `tz` dependency

## Error Messages

```
CLI config not found. Create ~/.config/wisp/cli.yaml with:
  timezone: "Europe/London"

timezone not set in ~/.config/wisp/cli.yaml

Invalid timezone 'Foo/Bar'. Use IANA format like 'Europe/London' or 'America/New_York'
```
