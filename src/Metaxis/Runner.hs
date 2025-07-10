{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metaxis.Runner where

import Metaxis.Class
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Console.ANSI
import Data.List (sort, isInfixOf)
import Control.Monad (forM_, when)
import Data.Text (pack, unpack)
import Data.Proxy

runMigrate :: forall b. MigrationBackend b => b -> FilePath -> IO ()
runMigrate backend migrationPath = do
  conn <- connectBackend backend
  let proxy = Proxy :: Proxy b
  ensureSchemaTable proxy conn
  files <- sort . filter isUp <$> listDirectory migrationPath
  putStrLn $ "Files to migrate: " ++ show files
  applied <- getAppliedMigrations proxy conn
  forM_ (filter (`notElem` applied) files) $ \f -> do
    sql <- TIO.readFile (migrationPath </> f)
    let cleanedSql = T.unlines . filter (not . T.isPrefixOf "--") $ T.lines sql
    applyMigration proxy conn f cleanedSql
    color Green ("Applied: " ++ f)

runRollback :: forall b. MigrationBackend b => b -> FilePath -> IO ()
runRollback backend migrationPath = do
  conn <- connectBackend backend
  let proxy = Proxy :: Proxy b
  ensureSchemaTable proxy conn
  applied <- getAppliedMigrations proxy conn
  case reverse applied of
    (f : _) -> do
      let down = dropExtension f <.> "down.sql"
      exists <- doesFileExist (migrationPath </> down)
      when exists $ do
        sql <- TIO.readFile (migrationPath </> down)
        let cleanedSql = T.unlines . filter (not . T.isPrefixOf "--") $ T.lines sql -- Remove comments
        putStrLn $ "Executing rollback: " ++ unpack cleanedSql
        rollbackMigration proxy conn f cleanedSql -- Pass the entire cleaned SQL to rollbackMigration
        deleteMigration proxy conn f
        color Yellow ("Rolled back: " ++ f)
    [] -> putStrLn "Nothing to roll back."

isUp f = takeExtension f == ".sql" && not ("down" `isInfixOf` f)

color c msg = setSGR [SetColor Foreground Vivid c] >> putStrLn msg >> setSGR [Reset]
