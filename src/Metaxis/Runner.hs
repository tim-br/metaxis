{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Metaxis.Runner where

import Metaxis.Class
import Data.Proxy
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import System.Console.ANSI
import Data.List (sort, isInfixOf)
import Control.Monad (forM_, when)


runMigrate :: forall b. MigrationBackend b => b -> IO ()
runMigrate backend = do
  conn <- connectBackend backend
  let proxy = Proxy :: Proxy b
  ensureSchemaTable proxy conn
  files <- sort . filter isUp <$> listDirectory "migrations"
  applied <- getAppliedMigrations proxy conn
  forM_ (filter (`notElem` applied) files) $ \f -> do
    sql <- TIO.readFile ("migrations" </> f)
    applyMigration proxy conn f sql
    color Green ("Applied: " ++ f)

runRollback :: forall b. MigrationBackend b => b -> IO ()
runRollback backend = do
  conn <- connectBackend backend
  let proxy = Proxy :: Proxy b
  ensureSchemaTable proxy conn
  applied <- getAppliedMigrations proxy conn
  case reverse applied of
    (f : _) -> do
      let down = dropExtension f <.> "down.sql"
      exists <- doesFileExist ("migrations" </> down)
      when exists $ do
        sql <- TIO.readFile ("migrations" </> down)
        rollbackMigration proxy conn f sql
        deleteMigration proxy conn f
        color Yellow ("Rolled back: " ++ f)
    [] -> putStrLn "Nothing to roll back."

isUp f = takeExtension f == ".sql" && not ("down" `isInfixOf` f)

color c msg = setSGR [SetColor Foreground Vivid c] >> putStrLn msg >> setSGR [Reset]
