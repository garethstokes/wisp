-- 007_add_notified_at.sql
ALTER TABLE activities ADD COLUMN IF NOT EXISTS notified_at TIMESTAMPTZ;
