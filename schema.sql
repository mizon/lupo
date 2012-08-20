CREATE TABLE entries (
    id INTEGER PRIMARY KEY,
    created_at TEXT NOT NULL,
    modified_at TEXT NOT NULL,
    day TEXT NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL
);

CREATE INDEX entries_day_id_index ON entries (day);
