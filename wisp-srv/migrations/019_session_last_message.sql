-- 019_session_last_message.sql
-- Track when last message was added for session resumption logic

-- Add column if it doesn't exist
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'sessions' AND column_name = 'last_message_at'
  ) THEN
    ALTER TABLE sessions ADD COLUMN last_message_at TIMESTAMPTZ;
  END IF;
END $$;

-- Backfill existing sessions where null
UPDATE sessions SET last_message_at = created_at WHERE last_message_at IS NULL;

-- Make non-nullable with default
ALTER TABLE sessions ALTER COLUMN last_message_at SET NOT NULL;
ALTER TABLE sessions ALTER COLUMN last_message_at SET DEFAULT now();

-- Index for efficient "recent active session" queries (idempotent)
CREATE INDEX IF NOT EXISTS idx_sessions_last_message ON sessions(agent_id, last_message_at DESC)
  WHERE ended_at IS NULL;
