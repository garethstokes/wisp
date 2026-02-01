-- migrations/001_entities.sql

-- Versioned JSON entity storage
-- Each update creates a new version (append-only)

create table if not exists entities (
  entity_type text not null,
  entity_id text not null,
  entity_version int not null,
  created_at timestamptz not null default now(),
  payload jsonb not null,
  primary key (entity_type, entity_id, entity_version)
);

-- Index for efficient "get latest version" queries
create index if not exists entities_latest
  on entities (entity_type, entity_id, entity_version desc);

-- Ensure schema name in JSON matches entity_type
alter table entities
add constraint entities_schema_name_matches
check ((payload #>> '{schema,name}') = entity_type);

-- Trigger function to auto-increment version per entity
create or replace function next_entity_version()
returns trigger as $$
begin
  select coalesce(max(entity_version), 0) + 1
  into new.entity_version
  from entities
  where entity_type = new.entity_type
    and entity_id = new.entity_id;
  return new;
end;
$$ language plpgsql;

-- Apply trigger for auto-versioning when version is null
drop trigger if exists entities_version_trigger on entities;
create trigger entities_version_trigger
before insert on entities
for each row
when (new.entity_version is null)
execute function next_entity_version();
