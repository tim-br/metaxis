{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metaxis.Postgres where

import Metaxis.Class
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import qualified Data.Text.Encoding as T
import Data.Proxy
import Control.Monad (void)

newtype Postgres = Postgres ConnectInfo

type instance ConnFor Postgres = Connection

instance MigrationBackend Postgres where
  connectBackend (Postgres info) = connect info

  ensureSchemaTable (_ :: Proxy Postgres) conn =
    void $ execute_ conn
      "CREATE TABLE IF NOT EXISTS schema_migrations (version TEXT PRIMARY KEY, applied_at TIMESTAMP DEFAULT now())"

  getAppliedMigrations (_ :: Proxy Postgres) conn =
    map fromOnly <$> query_ conn "SELECT version FROM schema_migrations"

  applyMigration (_ :: Proxy Postgres) conn fname sql = do
    execute_ conn (Query $ T.encodeUtf8 sql)
    void $ execute conn "INSERT INTO schema_migrations (version) VALUES (?)" (Only fname)

  rollbackMigration (_ :: Proxy Postgres) conn _ sql =
    void $ execute_ conn (Query $ T.encodeUtf8 sql)

  deleteMigration (_ :: Proxy Postgres) conn fname =
    void $ execute conn "DELETE FROM schema_migrations WHERE version = ?" (Only fname)
