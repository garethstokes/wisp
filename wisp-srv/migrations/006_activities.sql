-- migrations/006_activities.sql

-- Activities table for storing emails and calendar events

create table if not exists activities (
  id text primary key,
  source text not null,           -- 'email', 'calendar'
  source_id text not null,        -- Gmail message ID, Calendar event ID
  raw jsonb not null,             -- Original API response
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),

  -- Classification (populated later by LLM)
  personas text[],
  activity_type text,
  urgency text,
  autonomy_tier int,
  confidence float,
  status text not null default 'pending',

  -- Content
  title text,
  summary text,
  sender_email text,
  person_id text,

  -- Calendar-specific
  starts_at timestamptz,
  ends_at timestamptz
);

-- Prevent duplicate imports
create unique index if not exists activities_source_unique
  on activities (source, source_id);

-- Index for status queries
create index if not exists activities_by_status
  on activities (status, created_at desc);

-- Index for person queries
create index if not exists activities_by_person
  on activities (person_id, created_at desc);

-- Index for date range queries (calendar)
create index if not exists activities_by_date
  on activities (starts_at, ends_at)
  where starts_at is not null;
