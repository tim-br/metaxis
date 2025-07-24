{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Char (toLower)
import Options.Applicative
import Metaxis.Runner
import Metaxis.Class
import System.IO
import Data.Word (Word16)
import Data.Yaml (decodeFileThrow)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Metaxis.Config (Config(..), mergeConfig, defaultConfig, Backend(..)) -- Use Backend from Metaxis.Config

#ifdef POSTGRES_ENABLED
import Metaxis.Postgres
import Database.PostgreSQL.Simple (ConnectInfo, defaultConnectInfo) -- Explicitly import ConnectInfo
#endif

#ifdef SQLITE_ENABLED
import Metaxis.Sqlite
#endif

data Command = Migrate | Rollback

cliOptions :: Parser (Maybe FilePath, Maybe Backend, Maybe String, Maybe Word16, Maybe String, Maybe String, Maybe String)
cliOptions =
  (,,,,,,)
    <$> optional (strOption (long "migration-dir" <> metavar "DIR" <> help "Migration directory"))
    <*> optional (option auto (long "backend" <> metavar "postgres|sqlite" <> help "Backend"))
    <*> optional (strOption (long "host"     <> metavar "HOST"     <> help "PostgreSQL host"))
    <*> optional (option auto (long "port"   <> metavar "PORT"     <> help "PostgreSQL port"))
    <*> optional (strOption (long "user"     <> metavar "USER"     <> help "PostgreSQL user"))
    <*> optional (strOption (long "password" <> metavar "PASSWORD" <> help "PostgreSQL password"))
    <*> optional (strOption (long "database" <> metavar "DATABASE" <> help "PostgreSQL database"))

commandParser :: Parser Command
commandParser = subparser
  ( command "migrate" (info (pure Migrate) (progDesc "Apply migrations"))
 <> command "rollback" (info (pure Rollback) (progDesc "Roll back last migration"))
  )

main :: IO ()
main = do
  configExists <- doesFileExist "config.yaml"
  config <- if configExists then decodeFileThrow "config.yaml" else pure defaultConfig
  (cli, cmd) <- execParser (info ((,) <$> cliOptions <*> commandParser <**> helper) fullDesc)
  let (dir, backend, conn) = mergeConfig config cli
  run dir backend conn cmd

run :: FilePath -> Backend -> Either FilePath ConnectInfo -> Command -> IO ()
run dir PG (Right conn) Migrate =
#ifdef POSTGRES_ENABLED
  runMigrate (Postgres conn) dir
#else
  putStrLn "PostgreSQL not enabled at build time."
#endif

run dir PG (Right conn) Rollback =
#ifdef POSTGRES_ENABLED
  runRollback (Postgres conn) dir
#else
  putStrLn "PostgreSQL not enabled at build time."
#endif

run dir SQLITE (Left sqlitePath) Migrate =
#ifdef SQLITE_ENABLED
  runMigrate (Sqlite sqlitePath) dir
#else
  putStrLn "SQLite not enabled at build time."
#endif

run dir SQLITE (Left sqlitePath) Rollback =
#ifdef SQLITE_ENABLED
  runRollback (Sqlite sqlitePath) dir
#else
  putStrLn "SQLite not enabled at build time."
#endif

run _ _ _ _ = putStrLn "Invalid configuration or command."
