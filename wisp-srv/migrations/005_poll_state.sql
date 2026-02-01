-- migrations/005_poll_state.sql

-- Track polling progress for Gmail and Calendar

create table if not exists poll_state (
  source text primary key,
  last_poll_at timestamptz not null,
  cursor text
);

-- Pre-populate with sources
insert into poll_state (source, last_poll_at, cursor)
values
  ('gmail', now(), null),
  ('calendar', now(), null)
on conflict (source) do nothing;
