{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Char (toLower)
import Data.Word (Word16)
import Options.Applicative
import Metaxis.Runner
import Metaxis.Class
import System.IO
import Data.Yaml (decodeFileThrow)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import System.Exit (die)

#ifdef POSTGRES_ENABLED
import Metaxis.Postgres
import Database.PostgreSQL.Simple (ConnectInfo(..))
#endif

#ifdef SQLITE_ENABLED
import Metaxis.Sqlite
#endif

data Backend = PG | SQLITE deriving (Show, Eq)
data Command = Migrate FilePath Backend ConnectInfo | Rollback FilePath Backend ConnectInfo

data Config = Config
  { migrationDir :: Maybe FilePath
  , backend :: Maybe String
#ifdef POSTGRES_ENABLED
  , postgres :: Maybe ConnectInfo
#endif
#ifdef SQLITE_ENABLED
  , sqlite :: Maybe FilePath
#endif
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "migrationDir"
    <*> v .:? "backend"
#ifdef POSTGRES_ENABLED
    <*> v .:? "postgres"
#endif
#ifdef SQLITE_ENABLED
    <*> v .:? "sqlite"
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
  readsPrec _ value = case map toLower value of
    "postgres" -> [(PG, "")]
    "sqlite"   -> [(SQLITE, "")]
    _          -> []

cliOptions :: Parser (Bool, Maybe FilePath, Maybe Backend,
                      Maybe String, Maybe Word16, Maybe String, Maybe String, Maybe String)
cliOptions =
  (,,,,,,,)
    <$> hsubparser
          ( command "migrate"  (info (pure True)  (progDesc "Apply migrations"))
         <> command "rollback" (info (pure False) (progDesc "Roll back last migration"))
          )
    <*> optional (strOption (long "migration-dir" <> metavar "DIR" <> help "Migration directory"))
    <*> optional (option auto (long "backend" <> metavar "pg|sqlite" <> help "Backend"))
    <*> optional (strOption (long "host"     <> metavar "HOST"     <> help "PostgreSQL host"))
    <*> optional (option auto (long "port" <> metavar "PORT" <> help "PostgreSQL port"))
    <*> optional (strOption (long "user"     <> metavar "USER"     <> help "PostgreSQL user"))
    <*> optional (strOption (long "password" <> metavar "PASSWORD" <> help "PostgreSQL password"))
    <*> optional (strOption (long "database" <> metavar "DATABASE" <> help "PostgreSQL database"))


main :: IO ()
main = do
  configExists <- doesFileExist "config.yaml"
  config <- if configExists then decodeFileThrow "config.yaml" else pure defaultConfig
  (isMigrate, cliDir, cliBackend, cliHost, cliPort, cliUser, cliPass, cliDB) <- execParser (info (cliOptions <**> helper) fullDesc)

  let dir = fromMaybe (fromMaybe (error "Missing migrationDir") (migrationDir config)) cliDir
  let configBackend = backend config >>= safeReadBackend
      resolvedBackend = fromMaybe (fromMaybe (error "Missing backend") configBackend) cliBackend



#ifdef POSTGRES_ENABLED
  let configConn = postgres config
      merge cliVal getField = maybe (fmap getField configConn) Just cliVal
      conn = ConnectInfo
              (fromMaybe (dieMissing "host")     $ merge cliHost connectHost)
              (fromMaybe (dieMissing "port")     $ merge cliPort connectPort)
              (fromMaybe (dieMissing "user")     $ merge cliUser connectUser)
              (fromMaybe (dieMissing "password") $ merge cliPass connectPassword)
              (fromMaybe (dieMissing "database") $ merge cliDB connectDatabase)
#endif

#ifdef SQLITE_ENABLED
  let sqliteFile = fromMaybe (error "Missing SQLite db-name") (sqlite config)
      conn = ConnectInfo "" 0 "" "" sqliteFile
#endif

  let cmd = if isMigrate then Migrate dir resolvedBackend conn else Rollback dir resolvedBackend conn
  run config cmd

safeReadBackend :: String -> Maybe Backend
safeReadBackend s = case reads (map toLower s) of
  [(b, "")] -> Just b
  _         -> Nothing


run :: Config -> Command -> IO ()
run _ (Migrate dir PG conn) =
#ifdef POSTGRES_ENABLED
  runMigrate (Postgres conn) dir
#else
  putStrLn "PostgreSQL not enabled."
#endif

run _ (Rollback dir PG conn) =
#ifdef POSTGRES_ENABLED
  runRollback (Postgres conn) dir
#else
  putStrLn "PostgreSQL not enabled."
#endif

run _ (Migrate dir SQLITE conn) =
#ifdef SQLITE_ENABLED
  runMigrate (Sqlite (connectDatabase conn)) dir
#else
  putStrLn "SQLite not enabled."
#endif

run _ (Rollback dir SQLITE conn) =
#ifdef SQLITE_ENABLED
  runRollback (Sqlite (connectDatabase conn)) dir
#else
  putStrLn "SQLite not enabled."
#endif

defaultConfig :: Config
defaultConfig = Config Nothing Nothing
#ifdef POSTGRES_ENABLED
  Nothing
#endif
#ifdef SQLITE_ENABLED
  Nothing
#endif

dieMissing :: String -> a
dieMissing key = error $ "Missing required connection field: " ++ key
