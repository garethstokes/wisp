-- migrations/004_auth.sql

-- OAuth token storage for Google API access

create table if not exists auth_tokens (
  id text primary key,
  provider text not null,
  access_token text not null,
  refresh_token text not null,
  expires_at timestamptz not null,
  scopes text[] not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Only one token per provider
create unique index if not exists auth_tokens_provider_unique
  on auth_tokens (provider);
