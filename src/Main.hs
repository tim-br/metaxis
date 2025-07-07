{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}


module Main where

import Data.Char (toLower)
import Options.Applicative
import Metaxis.Runner
import Metaxis.Class
import System.IO

#ifdef POSTGRES_ENABLED
import Metaxis.Postgres
import Database.PostgreSQL.Simple (defaultConnectInfo)
#endif

#ifdef SQLITE_ENABLED
import Metaxis.Sqlite
#endif

data Backend = PG | SQLITE deriving (Show)
data Command = Migrate Backend | Rollback Backend

instance Read Backend where
  readsPrec _ value =
    case map toLower value of
      "pg"     -> [(PG, "")]
      "sqlite" -> [(SQLITE, "")]
      _        -> []


parser :: Parser Command
parser = subparser
  ( command "migrate" (info (Migrate <$> backendArg) (progDesc "Apply migrations"))
 <> command "rollback" (info (Rollback <$> backendArg) (progDesc "Roll back last"))
  )
  where
    backendArg = option auto (long "backend" <> metavar "pg|sqlite")

main :: IO ()
main = execParser (info (parser <**> helper) fullDesc) >>= run

run (Migrate PG) =
#ifdef POSTGRES_ENABLED
  runMigrate (Postgres defaultConnectInfo)
#else
  putStrLn "PostgreSQL not enabled at build time."
#endif

run (Rollback PG) =
#ifdef POSTGRES_ENABLED
  runRollback (Postgres defaultConnectInfo)
#else
  putStrLn "PostgreSQL not enabled at build time."
#endif

run (Migrate SQLITE) =
#ifdef SQLITE_ENABLED
  runMigrate (Sqlite "dev.sqlite3")
#else
  putStrLn "SQLite not enabled at build time."
#endif

run (Rollback SQLITE) =
#ifdef SQLITE_ENABLED
  runRollback (Sqlite "dev.sqlite3")
#else
  putStrLn "SQLite not enabled at build time."
#endif
