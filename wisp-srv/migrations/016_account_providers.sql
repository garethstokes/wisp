-- 016_account_providers.sql
-- Add provider support to accounts table

-- Add provider column (default 'google' for existing accounts)
ALTER TABLE accounts ADD COLUMN provider TEXT NOT NULL DEFAULT 'google';

-- Add details JSONB column for provider-specific data
ALTER TABLE accounts ADD COLUMN details JSONB NOT NULL DEFAULT '{}';

-- Migrate existing email data to details JSON
UPDATE accounts SET details = jsonb_build_object('email', email);

-- Drop the email column
ALTER TABLE accounts DROP COLUMN email;

-- Create index on provider for filtering
CREATE INDEX idx_accounts_provider ON accounts(provider);
