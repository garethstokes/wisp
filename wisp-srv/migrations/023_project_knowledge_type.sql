-- 023_project_knowledge_type.sql
-- Add project_knowledge document type for Librarian skill

-- Drop and recreate the check constraint to include project_knowledge
ALTER TABLE documents DROP CONSTRAINT IF EXISTS documents_type_check;
ALTER TABLE documents ADD CONSTRAINT documents_type_check
  CHECK (type IN ('project', 'note', 'preference', 'project_knowledge'));
