-- 022_document_parent_id.sql
-- Add parent_id for hierarchical document relationships (e.g., project -> knowledge docs)

ALTER TABLE documents ADD COLUMN IF NOT EXISTS parent_id TEXT REFERENCES documents(id);
CREATE INDEX IF NOT EXISTS idx_documents_parent ON documents(parent_id);
