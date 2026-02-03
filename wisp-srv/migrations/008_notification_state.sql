-- 008_notification_state.sql
CREATE TABLE IF NOT EXISTS notification_state (
  id TEXT PRIMARY KEY DEFAULT 'singleton',
  last_notification_at TIMESTAMPTZ
);

-- Insert initial row
INSERT INTO notification_state (id, last_notification_at)
VALUES ('singleton', NULL)
ON CONFLICT (id) DO NOTHING;
