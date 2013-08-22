CREATE TABLE entries
  ( id INTEGER PRIMARY KEY
  , created_at TEXT NOT NULL
  , modified_at TEXT NOT NULL
  , day TEXT NOT NULL
  , title TEXT NOT NULL
  , body TEXT NOT NULL
  );

CREATE TABLE comments
  ( id INTEGER PRIMARY KEY
  , created_at TEXT NOT NULL
  , modified_at TEXT NOT NULL
  , day TEXT NOT NULL
  , name TEXT NOT NULL
  , body TEXT NOT NULL
  );

CREATE TABLE notice
  ( id INTEGER PRIMARY KEY
  , token TEXT NOT NULL
  , message TEXT NOT NULL
  );

CREATE INDEX entries_day_index ON entries (day);
CREATE INDEX comments_day_index ON comments (day);
CREATE INDEX notice_token_index ON notice (token);
