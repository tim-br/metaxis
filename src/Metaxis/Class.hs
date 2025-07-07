{-# LANGUAGE TypeFamilies #-}

module Metaxis.Class where

import Data.Text (Text)
import Data.Proxy

type family ConnFor b

class MigrationBackend b where
  connectBackend       :: b -> IO (ConnFor b)

  ensureSchemaTable    :: Proxy b -> ConnFor b -> IO ()
  getAppliedMigrations :: Proxy b -> ConnFor b -> IO [FilePath]
  applyMigration       :: Proxy b -> ConnFor b -> FilePath -> Text -> IO ()
  rollbackMigration    :: Proxy b -> ConnFor b -> FilePath -> Text -> IO ()
  deleteMigration      :: Proxy b -> ConnFor b -> FilePath -> IO ()
