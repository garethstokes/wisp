-- Seed data for agents and skills
-- Run after migrations to set up initial agent and skill prompts
--
-- Usage: psql -d wisp -f seeds/001_agents_and_skills.sql
--
-- Prerequisites: A tenant must exist. Create one with: wisp tenant create "My Wisp"

-- Get the first tenant (or fail if none exists)
DO $$
DECLARE
  v_tenant_id uuid;
  v_account_id text;
BEGIN
  -- Get the first tenant
  SELECT id INTO v_tenant_id FROM tenants ORDER BY created_at LIMIT 1;

  IF v_tenant_id IS NULL THEN
    RAISE EXCEPTION 'No tenant found. Create one first with: wisp tenant create "Your Name"';
  END IF;

  -- Get an account belonging to this tenant (for the account_id FK)
  SELECT id INTO v_account_id FROM accounts WHERE tenant_id = v_tenant_id LIMIT 1;

  IF v_account_id IS NULL THEN
    RAISE EXCEPTION 'No accounts linked to tenant. Link accounts first.';
  END IF;

  -- Seed default agent: wisp
  INSERT INTO activities (id, account_id, tenant_id, source, source_id, raw, title, status, tags, created_at)
  VALUES (
    'seed-agent-wisp',
    v_account_id,
    v_tenant_id,
    'note',
    'seed-agent-wisp',
    '{"personality_seed": "Helpful, concise, and proactive. You anticipate needs and offer actionable suggestions.", "active_skill": null}',
    'Wisp - Default Agent',
    'processed',
    ARRAY['agent:wisp'],
    NOW()
  ) ON CONFLICT (account_id, source, source_id) DO UPDATE SET
    raw = EXCLUDED.raw,
    title = EXCLUDED.title,
    tags = EXCLUDED.tags,
    tenant_id = EXCLUDED.tenant_id;

  -- Note: Skills are activated at runtime via activate_skill tool or API.
  -- There is ONE agent (wisp) that can dynamically switch between skills.
  -- We do NOT create separate agents for each skill (e.g., no wisp/concierge).

  -- Seed concierge skill prompt
  INSERT INTO activities (id, account_id, tenant_id, source, source_id, raw, title, status, tags, created_at)
  VALUES (
    'seed-skill-concierge',
    v_account_id,
    v_tenant_id,
    'note',
    'seed-skill-concierge',
    '{}',
    'You are helping manage email and communications intake.

## Your Role

Classify, route, and manage incoming messages. Help the user understand what needs attention and what can be handled automatically.

## Tools

- update_activities: Update activity status or classification
- query_activities: Fetch activities by filter (status, type, date range)
- query_people: Look up contacts by name or email

## Guidelines

- Be concise and action-oriented
- Prioritize urgent items clearly
- Ask before taking irreversible actions
- Group similar items when presenting summaries
- Explain your classification reasoning when asked',
    'processed',
    ARRAY['skill:concierge', 'prompt'],
    NOW()
  ) ON CONFLICT (account_id, source, source_id) DO UPDATE SET
    title = EXCLUDED.title,
    tags = EXCLUDED.tags,
    tenant_id = EXCLUDED.tenant_id;

  -- Seed scheduler skill prompt
  INSERT INTO activities (id, account_id, tenant_id, source, source_id, raw, title, status, tags, created_at)
  VALUES (
    'seed-skill-scheduler',
    v_account_id,
    v_tenant_id,
    'note',
    'seed-skill-scheduler',
    '{}',
    'You are helping manage calendar and scheduling.

## Your Role

Help the user understand their schedule, find available time, and coordinate events.

## Tools

- query_calendar: Get calendar events for a date range
- find_free_slots: Find available time slots given constraints

## Guidelines

- Always consider time zones when discussing times
- Check for conflicts before suggesting meeting times
- Respect buffer time between meetings (default 15 minutes)
- Present options clearly with trade-offs
- Be mindful of work hours and quiet times',
    'processed',
    ARRAY['skill:scheduler', 'prompt'],
    NOW()
  ) ON CONFLICT (account_id, source, source_id) DO UPDATE SET
    title = EXCLUDED.title,
    tags = EXCLUDED.tags,
    tenant_id = EXCLUDED.tenant_id;

  -- Seed insights skill prompt
  INSERT INTO activities (id, account_id, tenant_id, source, source_id, raw, title, status, tags, created_at)
  VALUES (
    'seed-skill-insights',
    v_account_id,
    v_tenant_id,
    'note',
    'seed-skill-insights',
    '{}',
    'You are helping analyze patterns and generate insights from activity data.

## Your Role

Search through activities, identify patterns, and provide actionable insights about communication and time usage.

## Tools

- search_activities: Full-text search across activities with optional filters
- get_activities: Fetch activities by filter (status, type, date range)
- get_summary: Statistical overview of recent activities
- get_people_insights: Analyze frequent contacts and interaction patterns

## Guidelines

- Look for patterns across time periods
- Surface actionable insights, not just raw data
- Be specific with recommendations
- Quantify observations when possible
- Highlight both positive patterns and areas for improvement',
    'processed',
    ARRAY['skill:insights', 'prompt'],
    NOW()
  ) ON CONFLICT (account_id, source, source_id) DO UPDATE SET
    title = EXCLUDED.title,
    tags = EXCLUDED.tags,
    tenant_id = EXCLUDED.tenant_id;

  RAISE NOTICE 'Seeded agents and skills for tenant %', v_tenant_id;
END $$;

-- Verify the seeds
SELECT id, title, tags, tenant_id FROM activities WHERE id LIKE 'seed-%';
