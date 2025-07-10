-- Migration to add a description column to the items table

ALTER TABLE items ADD COLUMN description TEXT;
