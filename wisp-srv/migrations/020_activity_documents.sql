-- 020_activity_documents.sql
-- Many-to-many relationship between activities and documents

CREATE TABLE activity_documents (
  activity_id TEXT NOT NULL REFERENCES activities(id) ON DELETE CASCADE,
  document_id TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
  relationship TEXT NOT NULL,  -- 'project', 'mentions', etc.
  confidence FLOAT,
  source TEXT NOT NULL CHECK (source IN ('classifier', 'user', 'reflection')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (activity_id, document_id, relationship)
);

CREATE INDEX idx_activity_documents_document ON activity_documents(document_id);
CREATE INDEX idx_activity_documents_activity ON activity_documents(activity_id);

-- Track dismissed project suggestions to avoid re-surfacing
CREATE TABLE dismissed_suggestions (
  id TEXT PRIMARY KEY,
  cluster_key TEXT NOT NULL UNIQUE,  -- e.g., "subject:phoenix"
  tenant_id UUID REFERENCES tenants(id),
  dismissed_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_dismissed_suggestions_tenant ON dismissed_suggestions(tenant_id);
