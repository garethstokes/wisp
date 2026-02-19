-- 018_documents.sql
-- Documents table for projects, notes, preferences

CREATE TABLE documents (
  id TEXT PRIMARY KEY,
  tenant_id UUID REFERENCES tenants(id),
  type TEXT NOT NULL CHECK (type IN ('project', 'note', 'preference')),
  data JSONB NOT NULL,
  tags TEXT[] NOT NULL DEFAULT '{}',
  confidence FLOAT,
  source TEXT,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  archived_at TIMESTAMPTZ,
  supersedes_id TEXT REFERENCES documents(id),
  last_activity_at TIMESTAMPTZ
);

CREATE INDEX idx_documents_tenant ON documents(tenant_id);
CREATE INDEX idx_documents_type ON documents(type);
CREATE INDEX idx_documents_tags ON documents USING GIN(tags);
CREATE INDEX idx_documents_active ON documents(active) WHERE active = TRUE;
CREATE INDEX idx_documents_supersedes ON documents(supersedes_id);

CREATE TABLE document_log (
  id TEXT PRIMARY KEY,
  document_id TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  activity_id TEXT REFERENCES activities(id) ON DELETE SET NULL,
  description TEXT,
  source TEXT NOT NULL CHECK (source IN ('agent', 'user', 'classifier', 'reflection')),
  agent_id TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_document_log_document ON document_log(document_id);
CREATE INDEX idx_document_log_activity ON document_log(activity_id);
CREATE INDEX idx_document_log_created ON document_log(created_at DESC);
