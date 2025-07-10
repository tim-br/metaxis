-- Migration to create the items table

CREATE TABLE items (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);