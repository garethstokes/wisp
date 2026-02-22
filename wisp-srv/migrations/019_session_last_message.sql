-- 019_session_last_message.sql
-- Track when last message was added for session resumption logic

ALTER TABLE sessions ADD COLUMN last_message_at TIMESTAMPTZ;

-- Backfill existing sessions
UPDATE sessions SET last_message_at = created_at;

-- Make non-nullable with default
ALTER TABLE sessions ALTER COLUMN last_message_at SET NOT NULL;
ALTER TABLE sessions ALTER COLUMN last_message_at SET DEFAULT now();

-- Index for efficient "recent active session" queries
CREATE INDEX idx_sessions_last_message ON sessions(agent_id, last_message_at DESC)
  WHERE ended_at IS NULL;
