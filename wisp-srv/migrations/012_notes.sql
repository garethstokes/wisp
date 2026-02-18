-- Add tags column to activities and parent_id for note supersession

ALTER TABLE activities ADD COLUMN tags TEXT[] DEFAULT '{}';
ALTER TABLE activities ADD COLUMN parent_id TEXT REFERENCES activities(id);

CREATE INDEX idx_activities_tags ON activities USING GIN (tags);
CREATE INDEX idx_activities_source_note ON activities(source) WHERE source = 'note';
