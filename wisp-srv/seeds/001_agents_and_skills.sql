-- Seed data for agents and skills
-- Run after migrations to set up initial agent and skill prompts
--
-- Usage: psql -d wisp -f seeds/001_agents_and_skills.sql
--
-- Note: These use 'default' as account_id. Update to real account IDs in production.

-- Seed default agent: wisp
-- An agent is a named persona stored as a note with tag "agent:NAME"
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags, created_at)
VALUES (
  'seed-agent-wisp',
  'default',
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
  tags = EXCLUDED.tags;

-- Seed concierge skill prompt
-- A skill prompt is stored as a note with tags [skill:NAME, prompt]
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags, created_at)
VALUES (
  'seed-skill-concierge',
  'default',
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
  tags = EXCLUDED.tags;

-- Seed scheduler skill prompt
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags, created_at)
VALUES (
  'seed-skill-scheduler',
  'default',
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
  tags = EXCLUDED.tags;

-- Seed insights skill prompt
INSERT INTO activities (id, account_id, source, source_id, raw, title, status, tags, created_at)
VALUES (
  'seed-skill-insights',
  'default',
  'note',
  'seed-skill-insights',
  '{}',
  'You are helping analyze patterns and generate insights from activity data.

## Your Role

Search through activities, identify patterns, and provide actionable insights about communication and time usage.

## Tools

- search_activities: Full-text search across activities with optional filters
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
  tags = EXCLUDED.tags;

-- Verify the seeds
SELECT id, title, tags FROM activities WHERE id LIKE 'seed-%';
