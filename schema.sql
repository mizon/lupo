PRAGMA foreign_keys = ON;

CREATE TABLE entries (
    id INTEGER PRIMARY KEY,
    day_id TEXT NOT NULL REFERENCES days (id),
    created_at TEXT NOT NULL,
    modified_at TEXT NOT NULL,
    title TEXT NOT NULL
);

CREATE TABLE days (
    id TEXT PRIMARY KEY
);

CREATE INDEX entries_day_id_index ON entries (day_id);
CREATE INDEX days_index ON days (id);
