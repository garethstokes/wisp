-- migrations/015_activities_tenant.sql

-- Add tenant_id to activities table
-- This allows notes/knowledge to be tenant-scoped rather than account-scoped
-- For activities from external sources (email, calendar), tenant_id is derived from account
-- For notes/knowledge (agents, skills), tenant_id is set directly

ALTER TABLE activities ADD COLUMN IF NOT EXISTS tenant_id uuid REFERENCES tenants(id);

-- Index for looking up activities by tenant
CREATE INDEX IF NOT EXISTS activities_tenant_id_idx ON activities(tenant_id);

-- Backfill tenant_id from account's tenant for existing activities
UPDATE activities a
SET tenant_id = acc.tenant_id
FROM accounts acc
WHERE a.account_id = acc.id
  AND a.tenant_id IS NULL
  AND acc.tenant_id IS NOT NULL;
