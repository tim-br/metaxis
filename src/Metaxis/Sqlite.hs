{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metaxis.Sqlite where

import Metaxis.Class
import Database.SQLite.Simple
    ( execute,
      execute_,
      open,
      query_,
      Only(Only, fromOnly),
      Connection,
      Query(Query) )
import qualified Data.Text as T
import Data.Proxy
import Control.Monad (void)

newtype Sqlite = Sqlite FilePath

type instance ConnFor Sqlite = Connection

instance MigrationBackend Sqlite where
  connectBackend (Sqlite path) = open path

  ensureSchemaTable (_ :: Proxy Sqlite) conn =
    void $ execute_ conn
      "CREATE TABLE IF NOT EXISTS schema_migrations (version TEXT PRIMARY KEY, applied_at DATETIME DEFAULT CURRENT_TIMESTAMP)"

  getAppliedMigrations (_ :: Proxy Sqlite) conn =
    map fromOnly <$> query_ conn "SELECT version FROM schema_migrations"

  applyMigration (_ :: Proxy Sqlite) conn fname sql = do
    let queries = filter (not . T.null) $ filter (not . T.isPrefixOf "--") $ map T.strip $ T.splitOn ";" sql
    mapM_ (execute_ conn . Query) queries -- Execute each valid query
    void $ execute conn "INSERT INTO schema_migrations (version) VALUES (?)" (Only fname)

  rollbackMigration (_ :: Proxy Sqlite) conn _ sql =
    void $ execute_ conn (Query sql)

  deleteMigration (_ :: Proxy Sqlite) conn fname =
    void $ execute conn "DELETE FROM schema_migrations WHERE version = ?" (Only fname)
