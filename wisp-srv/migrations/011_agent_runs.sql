-- Agent runs table
CREATE TABLE agent_runs (
  id TEXT PRIMARY KEY,
  parent_run_id TEXT REFERENCES agent_runs(id) ON DELETE SET NULL,
  agent_id TEXT NOT NULL,
  session_id TEXT,
  status TEXT NOT NULL DEFAULT 'running',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Run events table (append-only log)
CREATE TABLE run_events (
  id TEXT PRIMARY KEY,
  run_id TEXT NOT NULL REFERENCES agent_runs(id) ON DELETE CASCADE,
  parent_event_id TEXT REFERENCES run_events(id) ON DELETE SET NULL,
  event_seq INT NOT NULL,
  event_type TEXT NOT NULL,
  event_data JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

  UNIQUE (run_id, event_seq)
);

-- Indexes
CREATE INDEX idx_runs_session ON agent_runs(session_id) WHERE session_id IS NOT NULL;
CREATE INDEX idx_runs_parent ON agent_runs(parent_run_id) WHERE parent_run_id IS NOT NULL;
CREATE INDEX idx_runs_status ON agent_runs(status);
CREATE INDEX idx_runs_agent ON agent_runs(agent_id);
CREATE INDEX idx_events_run ON run_events(run_id, event_seq);
CREATE INDEX idx_events_parent ON run_events(parent_event_id) WHERE parent_event_id IS NOT NULL;
CREATE INDEX idx_events_type ON run_events(run_id, event_type);
