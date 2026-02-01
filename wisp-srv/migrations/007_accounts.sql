-- migrations/007_accounts.sql

-- Accounts table for multi-account support
create table if not exists accounts (
  id text primary key,
  email text not null unique,
  display_name text,
  created_at timestamptz not null default now()
);

-- Clean slate: drop existing poll_state and activities data
truncate poll_state;
truncate activities;

-- Add account_id to auth_tokens
alter table auth_tokens add column if not exists account_id text references accounts(id);

-- Drop old unique constraint and add new one
drop index if exists auth_tokens_provider_unique;
create unique index if not exists auth_tokens_account_provider_unique
  on auth_tokens (account_id, provider);

-- Add account_id to poll_state
alter table poll_state add column if not exists account_id text references accounts(id);

-- Change poll_state primary key to (account_id, source)
alter table poll_state drop constraint if exists poll_state_pkey;
alter table poll_state add primary key (account_id, source);

-- Add account_id to activities
alter table activities add column if not exists account_id text references accounts(id);

-- Drop old unique constraint and add new one
drop index if exists activities_source_unique;
create unique index if not exists activities_account_source_unique
  on activities (account_id, source, source_id);

-- Make account_id not null after migration (new inserts will have it)
-- Note: existing orphaned auth_tokens will need re-auth
