# Project Detail Card Layout

## Overview

Redesign the TUI project detail view from raw JSON dump to a polished card-based layout with navigable knowledge documents.

## Design

### Project Header Card

```
┌─────────────────────────────────────────────────────────────┐
│  LUNE                                              active   │
│  work project                                   project-lune│
├─────────────────────────────────────────────────────────────┤
│  267 activities [a]   •   Last active: 2 hours ago         │
│  Created: Feb 23, 2026                                      │
│                                                             │
│  Tags: lune                                                 │
│  Participants: gareth stokes <notifications@github.com>    │
└─────────────────────────────────────────────────────────────┘
```

Formatting rules:
- Project name: bold, uppercase
- Status: colored (green=active, dim=archived)
- Project ID: right-aligned, dimmed
- Activity count: plain number, `[a]` hotkey to view activities
- Last active: relative time ("2 hours ago", "yesterday", "3 days ago")
- Created: human date ("Feb 23, 2026")
- Tags: colored chips
- Participants: each on own line

### Knowledge Documents List

```
Knowledge Documents                    [j/k ↑↓ scroll, Enter view, Esc back]

  ▸ Architecture                                        updated 1 day ago
    Product Research                                    updated 1 day ago
    Roadmap                                            updated 1 day ago
    Activity Log                                       updated 1 day ago
```

- Selected item marked with `▸` and highlighted
- Relative timestamps for "updated"
- j/k/↑/↓ to move selection
- Enter to view document content
- Mouse scroll works on entire view

### Document Expanded View

```
[Esc to return to project]

┌─ Architecture ──────────────────────────────────────────────┐
│                                                             │
│  Users/Personas:                                            │
│  Developers building functional programming applications... │
│                                                             │
│  Testing:                                                   │
│  Comprehensive golden test infrastructure with...           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

- Full document content, scrollable
- Esc returns to project detail (document list)

## Implementation Tasks

1. **Add relative time formatting**
   - Create `Tui/Widgets/Time.hs` with `relativeTime :: UTCTime -> UTCTime -> Text`
   - Handle: "just now", "X minutes ago", "X hours ago", "yesterday", "X days ago", "Feb 23, 2026"

2. **Add new attributes to theme**
   - `statusActive` (green)
   - `statusArchived` (dim)
   - `tag` (cyan background or similar)
   - `dim` (gray/muted)
   - `cardBorder` (for box drawing)

3. **Create project header card widget**
   - `renderProjectCard :: Document -> UTCTime -> Widget Name`
   - Extract and format: name, type, status, ID, activity count, dates, tags, participants
   - Use box-drawing characters for border

4. **Add knowledge document selection state**
   - Add `ksDocSelected :: Int` to `KnowledgeState`
   - Track which knowledge doc is selected (separate from project list selection)

5. **Create knowledge document list widget**
   - `renderKnowledgeDocList :: [Document] -> Int -> UTCTime -> Widget Name`
   - Show list with selection indicator
   - Include "updated X ago" timestamps

6. **Add document expanded view state**
   - Add `ksDocExpanded :: Maybe Int` to `KnowledgeState`
   - When `Just idx`, show full document content instead of list

7. **Update event handling**
   - In project detail view: j/k/↑/↓ move doc selection
   - Enter expands selected doc
   - Esc from expanded doc returns to list
   - Esc from list returns to project list
   - `a` key navigates to Activities view (filtered by project tag)

8. **Wire up activities navigation**
   - Add `KnowledgeAction` variant for "view activities for tag"
   - Handle in Main.hs to switch view and filter
