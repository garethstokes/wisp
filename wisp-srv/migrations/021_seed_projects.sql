-- 021_seed_projects.sql
-- Bootstrap initial projects for the default tenant

-- Insert projects (idempotent using ON CONFLICT)
INSERT INTO documents (id, tenant_id, type, data, tags, confidence, source, active)
VALUES
  ('project-wisp', NULL, 'project',
   '{"name": "Wisp", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['wisp'], 1.0, 'user', TRUE),
  ('project-lune', NULL, 'project',
   '{"name": "Lune", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['lune'], 1.0, 'user', TRUE),
  ('project-superit', NULL, 'project',
   '{"name": "Super IT", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['superit', 'super-it'], 1.0, 'user', TRUE),
  ('project-paidright', NULL, 'project',
   '{"name": "PaidRight", "type": "work", "summary": "", "status": "active", "participants": [], "activity_count": 0}'::jsonb,
   ARRAY['paidright'], 1.0, 'user', TRUE)
ON CONFLICT (id) DO NOTHING;
