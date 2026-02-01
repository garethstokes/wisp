-- migrations/002_jobs.sql

-- PostgreSQL-backed job queue
-- Workers claim jobs with SELECT ... FOR UPDATE SKIP LOCKED

create table if not exists jobs (
  id text primary key,
  job_type text not null,
  status text not null default 'queued',
  payload jsonb not null,
  run_after timestamptz,
  attempts int not null default 0,
  max_attempts int not null default 3,
  claimed_at timestamptz,
  last_error text,
  created_at timestamptz not null default now()
);

-- Index for efficient job claiming
create index if not exists jobs_runnable
  on jobs (status, run_after)
  where status = 'queued';

-- Index for job type filtering
create index if not exists jobs_by_type
  on jobs (job_type, status);
