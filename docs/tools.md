# Tools

This file defines all domain tools and strict JSON contracts.
See prior sections for the full normative specification.

## Domains

- inbox_tools
- storage_tools
- comms_tools
- calendar_tools
- admin_tools
- memory_tools
- policy_tools
- time_tools
- control_plane_tools

## Contracts

### classify_activity (storage_tools)

Type: deterministic

Used by concierge agent to classify incoming activities.

**Input:**
```json
{
  "activity_id": "Text",
  "content": "Text",
  "title": "Text",
  "sender_email": "Text | null",
  "starts_at": "DateTime | null"
}
```

**Output:**
```json
{
  "activity_type": "request | information | action_required | fyi | event",
  "urgency": "low | normal | high",
  "autonomy_tier": "1-4",
  "confidence": "0.0-1.0",
  "personas": ["work", "personal", "..."],
  "reasoning": "Text - why this classification",
  "suggested_actions": ["action1", "action2"],
  "option_framing": "Text | null - how to present if surfaced"
}
```

**Notes:**
- `option_framing` should be null for tier 1-2 (items not surfaced)
- `reasoning` is stored in receipt for audit trail
- `suggested_actions` lists 1-3 concrete actions user could take

### update_activities (storage_tools)

Type: decision

Used by concierge chat to update activity status and/or classification.

**Input:**
```json
{
  "activity_ids": ["id1", "id2"],
  "updates": {
    "status": "pending | needs_review | quarantined | surfaced | processed | archived | null",
    "classification": {
      "activity_type": "Text | null",
      "urgency": "Text | null",
      "autonomy_tier": "Int | null",
      "confidence": "Float | null",
      "personas": "[Text] | null",
      "reasoning": "Text | null",
      "suggested_actions": "[Text] | null",
      "option_framing": "Text | null"
    }
  }
}
```

**Output:**
```json
{
  "updated_count": "Int",
  "activity_ids": ["id1", "id2"]
}
```

### query_activities (storage_tools)

Type: decision

Used to fetch activities by filter criteria.

**Input:**
```json
{
  "status": "Text | null",
  "limit": "Int | null (default 20)",
  "since": "DateTime | null",
  "before": "DateTime | null"
}
```

**Output:**
```json
{
  "activities": [Activity],
  "count": "Int"
}
```

### query_people (storage_tools)

Type: decision

Used to look up contacts.

**Input:**
```json
{
  "email": "Text | null",
  "search": "Text | null",
  "limit": "Int | null"
}
```

**Output:**
```json
{
  "people": [Person],
  "count": "Int"
}
```
