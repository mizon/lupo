CREATE TABLE entries
    ( id INTEGER PRIMARY KEY
    , created_at TEXT NOT NULL
    , modified_at TEXT NOT NULL
    , title TEXT NOT NULL
    );

CREATE INDEX entries_title_index ON entries (title);
