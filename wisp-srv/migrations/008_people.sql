-- migrations/008_people.sql

-- People table for contact management
create table if not exists people (
  id text primary key,
  email text unique not null,
  display_name text,
  personas text[],                  -- ['work'], ['home'], etc.
  relationship text,                -- 'colleague', 'investor', 'spouse', etc.
  organisation text,
  notes text,
  first_contact timestamptz,
  last_contact timestamptz,
  contact_count int not null default 0,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- Index for email lookups
create index if not exists people_by_email on people (email);

-- Add person_id FK to activities (column already exists from 006_activities.sql)
-- Just add the foreign key constraint
alter table activities
  add constraint activities_person_fk
  foreign key (person_id) references people(id);
