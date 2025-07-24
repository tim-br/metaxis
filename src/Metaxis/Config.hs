{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Metaxis.Config (Config(..), Backend(..), mergeConfig, safeReadBackend, defaultConfig) where

import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Data.Char (toLower)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Control.Applicative ((<|>)) -- Import <|> from Control.Applicative
#ifdef POSTGRES_ENABLED
import Database.PostgreSQL.Simple (ConnectInfo(..), connectHost, connectPort, connectUser, connectPassword, connectDatabase) -- Import ConnectInfo and related functions
#endif

data Backend = PG | SQLITE deriving (Show, Eq)

instance Read Backend where
  readsPrec _ value = case map toLower value of
    "postgres" -> [(PG, "")]
    "sqlite"   -> [(SQLITE, "")]
    _          -> []

#ifdef POSTGRES_ENABLED
instance FromJSON ConnectInfo where
  parseJSON = withObject "ConnectInfo" $ \v -> ConnectInfo
    <$> v .: "host"
    <*> v .: "port"
    <*> v .: "user"
    <*> v .: "password"
    <*> v .: "database"
#endif

data Config = Config
  { migrationDir :: Maybe FilePath
  , backend :: Maybe String
#ifdef SQLITE_ENABLED
  , sqlite :: Maybe FilePath
#endif
#ifdef POSTGRES_ENABLED
  , postgres :: Maybe ConnectInfo
#endif
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "migrationDir"
    <*> v .:? "backend"
#ifdef SQLITE_ENABLED
    <*> v .:? "sqlite"
#endif
#ifdef POSTGRES_ENABLED
    <*> v .:? "postgres"
#endif

safeReadBackend :: String -> Maybe Backend
safeReadBackend s = case reads (map toLower s) of
  [(b, "")] -> Just b
  _         -> Nothing

#ifdef POSTGRES_ENABLED
mergeConfig :: Config -> (Maybe FilePath, Maybe Backend, Maybe String, Maybe Word16, Maybe String, Maybe String, Maybe String)
            -> (FilePath, Backend, Either FilePath ConnectInfo)
#else
mergeConfig :: Config -> (Maybe FilePath, Maybe Backend, Maybe String, Maybe Word16, Maybe String, Maybe String, Maybe String)
            -> (FilePath, Backend, Either FilePath String)
#endif
mergeConfig config (cliDir, cliBackend, cliHost, cliPort, cliUser, cliPass, cliDB) =
  let dir = fromMaybe (fromMaybe (error "Missing migrationDir") (migrationDir config)) cliDir
      configBackend = backend config >>= safeReadBackend
      resolvedBackend = fromMaybe (fromMaybe (error "Missing backend") configBackend) cliBackend
      conn = case resolvedBackend of
        PG ->
#ifdef POSTGRES_ENABLED
          let configConn = postgres config
              --merge cliVal getField = maybe (fmap getField configConn) Just cliVal
              --merge cliVal getField = cliVal <|> (configConn >>= (Just . getField))
              merge cliVal getField = cliVal <|> (getField <$> configConn)


          in Right $ ConnectInfo
               (fromMaybe (error "Missing host")     $ merge cliHost connectHost)
               (fromMaybe (error "Missing port")     $ merge cliPort connectPort)
               (fromMaybe (error "Missing user")     $ merge cliUser connectUser)
               (fromMaybe (error "Missing password") $ merge cliPass connectPassword)
               (fromMaybe (error "Missing database") $ merge cliDB connectDatabase)
#else
          error "PostgreSQL backend is not enabled."
#endif
        SQLITE ->
#ifdef SQLITE_ENABLED
          let sqliteFile = fromMaybe (error "Missing SQLite db-name") (cliDB <|> sqlite config)
#else
          let sqliteFile = fromMaybe (error "Missing SQLite db-name") cliDB
#endif
          in Left sqliteFile
  in (dir, resolvedBackend, conn)

defaultConfig :: Config
defaultConfig = Config Nothing Nothing
#ifdef SQLITE_ENABLED
  Nothing
#endif
#ifdef POSTGRES_ENABLED
  Nothing
#endif
