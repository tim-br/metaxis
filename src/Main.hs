{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Char (toLower)
import Options.Applicative
import Metaxis.Runner
import Metaxis.Class
import System.IO
import Data.Yaml (decodeFileThrow, withObject, (.:))
import Data.Aeson (FromJSON(..))
import Control.Monad (when)

#ifdef POSTGRES_ENABLED
import Metaxis.Postgres
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
#endif

#ifdef SQLITE_ENABLED
import Metaxis.Sqlite
#endif

data Backend = PG | SQLITE deriving (Show)
data Command = Migrate Backend | Rollback Backend

data Config = Config
  { migrationDir :: FilePath
  , backend :: String
#ifdef POSTGRES_ENABLED
  , postgres :: ConnectInfo
#endif
#ifdef SQLITE_ENABLED
  , sqlite :: FilePath
#endif
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "migrationDir"
    <*> v .: "backend"
#ifdef POSTGRES_ENABLED
    <*> v .: "postgres"
#endif
#ifdef SQLITE_ENABLED
    <*> v .: "sqlite"
#endif

#ifdef POSTGRES_ENABLED
instance FromJSON ConnectInfo where
  parseJSON = withObject "ConnectInfo" $ \v -> ConnectInfo
    <$> v .: "host"
    <*> v .: "port"
    <*> v .: "user"
    <*> v .: "password"
    <*> v .: "database"
#endif

instance Read Backend where
  readsPrec _ value =
    case map toLower value of
      "postgres"     -> [(PG, "")]
      "sqlite" -> [(SQLITE, "")]
      _        -> error $ "Invalid backend: " ++ value ++ ". Expected 'postgres' or 'sqlite'."

parser :: Maybe String -> Parser Command
parser backendFromConfig = subparser
  ( command "migrate" (info (Migrate <$> backendArg) (progDesc "Apply migrations"))
 <> command "rollback" (info (Rollback <$> backendArg) (progDesc "Roll back last"))
  )
  where
    backendArg = case backendFromConfig of
      Just backend -> pure (read backend :: Backend)
      Nothing -> option auto (long "backend" <> metavar "pg|sqlite" <> help "Specify the backend (pg or sqlite)")

main :: IO ()
main = do
  config <- decodeFileThrow "config.yaml" :: IO Config
  let backendFromConfig = if null (backend config) then Nothing else Just (backend config)
  execParser (info (parser backendFromConfig <**> helper) fullDesc) >>= run config

run :: Config -> Command -> IO ()
run config (Migrate PG) =
#ifdef POSTGRES_ENABLED
  runMigrate (Postgres (postgres config)) (migrationDir config)
  --case postgres config of
  --  Just conn -> runMigrate (Postgres conn) (migrationDir config)
  --  Nothing -> putStrLn "PostgreSQL not enabled at build time."
#else
  putStrLn "PostgreSQL not enabled at build time."
#endif

run config (Rollback PG) =
#ifdef POSTGRES_ENABLED
  runRollback (Postgres (postgres config)) (migrationDir config)
  --case postgres config of
  --  Just conn -> runRollback (Postgres conn) (migrationDir config)
  --   Nothing -> putStrLn "PostgreSQL not enabled at build time."
#else
  putStrLn "PostgreSQL not enabled at build time."
#endif

run config (Migrate SQLITE) =
#ifdef SQLITE_ENABLED
  runMigrate (Sqlite (sqlite config)) (migrationDir config)
#else
  putStrLn "SQLite not enabled at build time."
#endif

run config (Rollback SQLITE) =
#ifdef SQLITE_ENABLED
  runRollback (Sqlite (sqlite config)) (migrationDir config)
#else
  putStrLn "SQLite not enabled at build time."
#endif