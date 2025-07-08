-- Rollback migration to remove the description column from the items table

ALTER TABLE items DROP COLUMN description;
