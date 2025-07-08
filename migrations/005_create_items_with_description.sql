-- Migration to add 10 items with descriptions to the items table

DO $$
BEGIN
  FOR i IN 1..10 LOOP
    INSERT INTO items (name, description) VALUES (
      'Item ' || i,
      'Description for Item ' || i
    );
  END LOOP;
END;
$$;
