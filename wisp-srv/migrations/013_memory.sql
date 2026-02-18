-- migrations/013_memory.sql

-- Agent memory: sessions, summaries, souls

create table if not exists sessions (
  id text primary key,
  agent_id text not null,
  messages jsonb not null default '[]',
  created_at timestamptz not null default now(),
  ended_at timestamptz,
  summarized boolean not null default false
);

create table if not exists summaries (
  id text primary key,
  agent_id text not null,
  session_ids text[] not null,
  content text not null,
  created_at timestamptz not null default now()
);

create table if not exists souls (
  agent_id text primary key,
  personality text not null default '',
  insights jsonb not null default '[]',
  updated_at timestamptz not null default now()
);

create index if not exists idx_sessions_agent on sessions(agent_id);
create index if not exists idx_sessions_created on sessions(created_at desc);
create index if not exists idx_sessions_not_summarized on sessions(agent_id) where not summarized;
create index if not exists idx_summaries_agent on summaries(agent_id);
