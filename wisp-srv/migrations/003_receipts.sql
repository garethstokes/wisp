-- migrations/003_receipts.sql

-- Audit log of all actions taken by Wisp

create table if not exists receipts (
  id text primary key,
  activity_id text not null,
  action_taken text not null,
  action_detail text,
  confidence float,
  created_at timestamptz not null default now()
);

-- Index for looking up receipts by activity
create index if not exists receipts_by_activity
  on receipts (activity_id);

-- Index for chronological queries
create index if not exists receipts_by_time
  on receipts (created_at desc);
