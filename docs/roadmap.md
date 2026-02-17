# Wisp Roadmap

This document outlines the current state and future direction of Wisp.

## Current State (v0.1)

### Working Features

- **Gmail Integration**: Polling, message ingestion, incremental sync
- **Calendar Integration**: Event polling, schedule queries
- **Multi-account Support**: Multiple Gmail/Calendar accounts
- **Classification Pipeline**: Automatic classification with confidence-based routing
- **Three Agents**: Concierge (activities), Scheduler (calendar), Insights (search)
- **Event Sourcing**: Full audit trail for agent runs
- **CLI**: Comprehensive command-line interface
- **HTTP API**: RESTful endpoints for all operations

### Known Limitations

- No web UI (CLI only)
- Single-user design
- No push notifications (polling only)
- Classification accuracy depends on prompt tuning
- No email sending (read-only Gmail access)

## Short-term Goals

### Improved Classification

- [ ] User feedback loop for classification corrections
- [ ] Per-user classification preferences
- [ ] Domain-specific classifiers (work vs personal)

### Better Calendar Integration

- [ ] Event creation (not just reading)
- [ ] Conflict detection and resolution
- [ ] Meeting preparation summaries

### CLI Enhancements

- [ ] Interactive mode (REPL-style)
- [ ] Rich terminal UI with colors and tables
- [ ] Autocompletion for commands

## Medium-term Goals

### Web Interface

- [ ] Dashboard showing inbox, calendar, insights
- [ ] Activity detail view with classification explanation
- [ ] Agent chat interface
- [ ] Settings and configuration UI

### Housekeeper Agent

- [ ] Automatic cleanup of old activities
- [ ] Archive management
- [ ] Storage optimization
- [ ] Scheduled maintenance tasks

### Smart Notifications

- [ ] Priority-based notification batching
- [ ] VIP sender alerts
- [ ] Configurable notification channels (email, webhook)
- [ ] Quiet hours enforcement

## Long-term Vision

### Multi-user Support

- [ ] User accounts and authentication
- [ ] Per-user configuration
- [ ] Shared calendars and activities

### Additional Integrations

- [ ] Slack/Teams messages
- [ ] Task managers (Todoist, Things)
- [ ] Note-taking apps (Notion, Obsidian)
- [ ] CRM integration

### Advanced AI Features

- [ ] Learning from user corrections
- [ ] Predictive scheduling
- [ ] Automated email drafts (with approval)
- [ ] Relationship insights and reminders

### Self-hosting Improvements

- [ ] Docker deployment
- [ ] Helm charts for Kubernetes
- [ ] SQLite option for single-user setups
- [ ] Reduced external dependencies

## Non-goals

Things Wisp explicitly won't do:

- **Auto-respond to emails**: Maintains user agency
- **Make decisions without consent**: Tier system ensures review gates
- **Collect telemetry**: No data leaves your server
- **Require cloud services**: Self-hosted by design

## Contributing

Contributions welcome! Areas where help is needed:

1. **Classification prompts**: Improving accuracy
2. **Test coverage**: More integration tests
3. **Documentation**: Examples and tutorials
4. **Agents**: New agent capabilities

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.
