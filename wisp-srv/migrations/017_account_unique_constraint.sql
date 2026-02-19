-- 017_account_unique_constraint.sql
-- Add unique constraint for account upsert

-- For Google accounts: unique on (provider, email from details)
-- For GitHub accounts: unique on (provider, username from details)
-- We use a computed expression that extracts the relevant identifier

-- Create unique index that extracts the identifier based on provider
CREATE UNIQUE INDEX idx_accounts_provider_identifier
ON accounts (
  provider,
  COALESCE(
    details->>'email',      -- Google accounts use email
    details->>'username'    -- GitHub accounts use username
  )
);
