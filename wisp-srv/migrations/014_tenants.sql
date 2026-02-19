-- migrations/014_tenants.sql

-- Tenants table - ownership boundary for agents, skills, knowledge
create table if not exists tenants (
  id uuid primary key default gen_random_uuid(),
  name text not null,
  created_at timestamptz not null default now()
);

-- Add tenant_id to accounts (an account belongs to a tenant)
alter table accounts add column if not exists tenant_id uuid references tenants(id);

-- Index for looking up accounts by tenant
create index if not exists accounts_tenant_id_idx on accounts(tenant_id);
