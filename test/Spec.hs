{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Metaxis.Class
import Metaxis.Sqlite
import qualified Data.Text as T
import System.Directory (removeFile, doesFileExist)
import Data.Proxy
import Database.SQLite.Simple (close)
import Control.Monad (when)

main :: IO ()
main = hspec $ do
  describe "Metaxis.Sqlite" $ do
    it "ensures schema table is created" $ do
      let backend = Sqlite "test.sqlite"
      conn <- connectBackend backend
      ensureSchemaTable (Proxy :: Proxy Sqlite) conn
      applied <- getAppliedMigrations (Proxy :: Proxy Sqlite) conn
      applied `shouldBe` []
      close conn
      cleanupSqlite "test.sqlite"

    it "applies a migration" $ do
      let backend = Sqlite "test.sqlite"
      conn <- connectBackend backend
      ensureSchemaTable (Proxy :: Proxy Sqlite) conn
      applyMigration (Proxy :: Proxy Sqlite) conn "001_test.sql" "CREATE TABLE test (id INTEGER PRIMARY KEY);"
      applied <- getAppliedMigrations (Proxy :: Proxy Sqlite) conn
      applied `shouldBe` ["001_test.sql"]
      close conn
      cleanupSqlite "test.sqlite"

cleanupSqlite :: FilePath -> IO ()
cleanupSqlite path = do
  exists <- doesFileExist path
  when exists $ removeFile path
