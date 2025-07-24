{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

module ConfigSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (evaluate, ErrorCall(..))
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Control.Applicative ((<|>))
import Metaxis.Config (Config(..), Backend(..), mergeConfig, safeReadBackend)

#ifdef POSTGRES_ENABLED
import Database.PostgreSQL.Simple (ConnectInfo(..), connectHost, connectPort, connectUser, connectPassword, connectDatabase)
#endif

data PartialConnectInfo = PartialConnectInfo
  { pcHost     :: Maybe String
  , pcPort     :: Maybe Word16
  , pcUser     :: Maybe String
  , pcPassword :: Maybe String
  , pcDb       :: Maybe String
  }

instance Show PartialConnectInfo where
  show pci =
    "PartialConnectInfo { host = "     ++ show (pcHost pci) ++
    ", port = "     ++ show (pcPort pci) ++
    ", user = "     ++ show (pcUser pci) ++
    ", password = " ++ show (pcPassword pci) ++
    ", database = " ++ show (pcDb pci) ++ " }"

toConnectInfo :: PartialConnectInfo -> Maybe ConnectInfo
toConnectInfo (PartialConnectInfo h p u pw db) =
  ConnectInfo <$> h <*> p <*> u <*> pw <*> db


-- Arbitrary instance for Backend
instance Arbitrary Backend where
  arbitrary = elements [PG, SQLITE]

#ifdef POSTGRES_ENABLED
instance Arbitrary ConnectInfo where
  arbitrary = ConnectInfo
    <$> elements ["localhost", "127.0.0.1", "db.example.com"]
    <*> elements [5432, 5433, 15432]
    <*> elements ["user", "admin", "postgres"]
    <*> elements ["pass", "secret", "1234"]
    <*> elements ["testdb", "appdb", "postgres"]
#endif

-- At most one missing field pair
genMaybePair :: (Arbitrary a, Eq a) => Gen (Maybe a, Maybe a)
genMaybePair = do
  cli <- arbitrary
  config <- arbitrary
  if cli == Nothing && config == Nothing
    then do
      x <- arbitrary
      oneof [pure (Just x, Nothing), pure (Nothing, Just x)]
    else return (cli, config)

genBackendStringMaybe :: Gen (Maybe String)
genBackendStringMaybe = frequency
  [ (1, return Nothing)
  , (1, return $ Just "")
  , (3, elements [Just "postgres", Just "sqlite"])
  ]

genBackendStringPair :: Gen (Maybe String, Maybe String)
genBackendStringPair = do
  cli    <- frequency [ (1, return Nothing)
                      , (3, elements [Just "postgres", Just "sqlite"]) ]
  config <- frequency [ (1, return Nothing)
                      , (1, return (Just ""))
                      , (3, elements [Just "postgres", Just "sqlite"]) ]
  return (cli, config)

#ifdef POSTGRES_ENABLED
-- At most one missing PostgreSQL field
genOneMissingPostgresField :: Gen (Maybe String, Maybe Word16, Maybe String, Maybe String, Maybe String, Maybe PartialConnectInfo)
genOneMissingPostgresField = do
  let allFields = [0..4]
  missing <- elements allFields

  let fieldOrMissing i val = if i == missing then Nothing else Just val

  cliHost     <- fieldOrMissing 0 <$> elements ["localhost"]
  cliPort     <- fieldOrMissing 1 <$> elements [5432]
  cliUser     <- fieldOrMissing 2 <$> elements ["admin"]
  cliPass     <- fieldOrMissing 3 <$> elements ["pw"]
  cliDb       <- fieldOrMissing 4 <$> elements ["db"]

  configHost     <- fieldOrMissing 0 <$> elements ["config-host"]
  configPort     <- fieldOrMissing 1 <$> elements [5433]
  configUser     <- fieldOrMissing 2 <$> elements ["config-user"]
  configPassword <- fieldOrMissing 3 <$> elements ["config-pass"]
  configDb       <- fieldOrMissing 4 <$> elements ["config-db"]

  let partialConfig = PartialConnectInfo
                        configHost configPort configUser configPassword configDb

  return (cliHost, cliPort, cliUser, cliPass, cliDb, Just partialConfig)

#endif

normalize :: Maybe String -> Maybe String
normalize (Just s) | null s = Nothing
normalize x = x

spec :: Spec
spec = do
  describe "mergeConfig" $ do

    it "uses CLI or config migrationDir, errors if both are missing" $
      forAll genMaybePair $ \(cliDir, configDir) ->
        let config = Config
              { migrationDir = configDir
              , backend = Just "sqlite"
#ifdef SQLITE_ENABLED
              , sqlite = Just "config.sqlite3"
#endif
#ifdef POSTGRES_ENABLED
              , postgres = Nothing
#endif
              }
            cliOptions = (cliDir, Just SQLITE, Nothing, Nothing, Nothing, Nothing, Just "cli.sqlite3")
        in monadicIO $ run do
          if cliDir == Nothing && configDir == Nothing
            then evaluate (mergeConfig config cliOptions)
              `shouldThrow` (\(ErrorCall m) -> m == "Missing migrationDir")
            else do
              let (mergedDir, _, _) = mergeConfig config cliOptions
              mergedDir `shouldBe` fromMaybe (fromMaybe (error "unreachable") configDir) cliDir

    it "uses CLI or config backend, errors if both are missing" $
      forAll genBackendStringPair $ \(cliBackendStr, configBackendStr) ->
        let cliParsed    = safeReadBackend =<< normalize cliBackendStr
            configParsed = safeReadBackend =<< normalize configBackendStr
            config = Config
              { migrationDir = Just "migrations"
              , backend = normalize configBackendStr
#ifdef SQLITE_ENABLED
              , sqlite = Just "config.sqlite3"
#endif
#ifdef POSTGRES_ENABLED
              , postgres = Nothing
#endif
              }
            cliOptions = (Just "migrations", cliParsed, Nothing, Nothing, Nothing, Nothing, Just "db.sqlite3")
        in monadicIO $ run do
          case (cliParsed, configParsed) of
            (Nothing, Nothing) ->
              evaluate (let (a, b, c) = mergeConfig config cliOptions in a `seq` b `seq` c `seq` ())
                `shouldThrow` (\(ErrorCall m) -> m == "Missing backend")
            _ -> do
              let (_, mergedBackend, _) = mergeConfig config cliOptions
              mergedBackend `shouldBe` fromMaybe (error "unreachable") (cliParsed <|> configParsed)


#ifdef SQLITE_ENABLED
    it "uses CLI or config sqlite db-name, errors if both are missing" $
      forAll genMaybePair $ \(cliDbName, configDbName) ->
        let config = Config
              { migrationDir = Just "migrations"
              , backend = Just "sqlite"
              , sqlite = configDbName
#ifdef POSTGRES_ENABLED
              , postgres = Nothing
#endif
              }
            cliOptions = (Just "migrations", Just SQLITE, Nothing, Nothing, Nothing, Nothing, cliDbName)
        in monadicIO $ run do
          if cliDbName == Nothing && configDbName == Nothing
            then evaluate (mergeConfig config cliOptions)
              `shouldThrow` (\(ErrorCall m) -> m == "Missing SQLite db-name")
            else do
              let (_, _, mergedConn) = mergeConfig config cliOptions
              mergedConn `shouldBe` Left (fromMaybe (error "unreachable") (cliDbName <|> configDbName))
#endif

#ifdef POSTGRES_ENABLED
    it "uses CLI or config Postgres fields, errors if one is missing" $
      --verbose $ 
      forAll genOneMissingPostgresField $ \(cliHost, cliPort, cliUser, cliPass, cliDb, configConn) ->
        let config = Config
              { migrationDir = Just "migrations"
              , backend = Just "postgres"
              , postgres = configConn >>= toConnectInfo 
#ifdef SQLITE_ENABLED
              , sqlite = Nothing
#endif
              }
            cliOptions = (Nothing, Just PG, cliHost, cliPort, cliUser, cliPass, cliDb)
            mergeGeneric :: Maybe a -> (ConnectInfo -> a) -> Maybe a
            mergeGeneric cliVal getter =
              cliVal <|> ( (toConnectInfo =<< configConn) >>= (Just . getter) )

            -- mergeGeneric cliVal getter = cliVal <|> (getter <$> configConn)
            
            --mergeGeneric cliVal getter = cliVal <|> (toConnectInfo configConn >>= (Just . getter))
            -- mergeGeneric cliVal getter = cliVal <|> (configConn >>= getter)

            -- mergeGeneric cliVal getter = cliVal <|> (configConn >>= toConnectInfo >>= (Just . getter))

            --mergeGeneric cliVal getter = cliVal <|> (getter <$> configConn)

            --mergeGeneric cliVal getter = cliVal <|> fmap getter configConn

        in monadicIO $ run do
          let missingFields =
                [ ("host",     mergeGeneric cliHost connectHost)
                , ("port",     fmap show $ mergeGeneric cliPort connectPort)
                , ("user",     mergeGeneric cliUser connectUser)
                , ("password", mergeGeneric cliPass connectPassword)
                , ("database", mergeGeneric cliDb  connectDatabase)
                ] 
              firstMissing = [ name | (name, Nothing) <- missingFields ]
          if null firstMissing
            then do
              let (_, _, Right final) = mergeConfig config cliOptions
              final `shouldBe` ConnectInfo
                (fromMaybe (error "missing") $ mergeGeneric cliHost connectHost)
                (fromMaybe (error "missing") $ mergeGeneric cliPort connectPort)
                (fromMaybe (error "missing") $ mergeGeneric cliUser connectUser)
                (fromMaybe (error "missing") $ mergeGeneric cliPass connectPassword)
                (fromMaybe (error "missing") $ mergeGeneric cliDb  connectDatabase)
            else do
              let err = "Missing " ++ head firstMissing
              let (_, _, Right conn) = mergeConfig config cliOptions
              case head firstMissing of
                "host"     -> evaluate (connectHost conn `seq` ())     `shouldThrow` (\(ErrorCall msg) -> msg == err)
                "port"     -> evaluate (connectPort conn `seq` ())     `shouldThrow` (\(ErrorCall msg) -> msg == err)
                "user"     -> evaluate (connectUser conn `seq` ())     `shouldThrow` (\(ErrorCall msg) -> msg == err)
                "password" -> connectPassword conn `shouldBe` "" 
                "database" -> evaluate (connectDatabase conn `seq` ()) `shouldThrow` (\(ErrorCall msg) -> msg == err)
                _          -> error "Unexpected field name"
              -- let err = "Missing " ++ head firstMissing
              -- liftIO $ do
              --   putStrLn "\n--- Postgres Field Combination ---"
              --   putStrLn $ "CLI Host:     " ++ show cliHost
              --   putStrLn $ "CLI Port:     " ++ show cliPort
              --   putStrLn $ "CLI User:     " ++ show cliUser
              --   putStrLn $ "CLI Pass:     " ++ show cliPass
              --   putStrLn $ "CLI DB:       " ++ show cliDb
              --   putStrLn $ "Config Conn:  " ++ show configConn
              --   putStrLn $ "Expected err: " ++ err
              -- -- evaluate (mergeConfig config cliOptions)
              -- let (a, b, Right conn) = mergeConfig config cliOptions
              -- evaluate (a `seq` b `seq` conn `seq` ())
              --   `shouldThrow` (\(ErrorCall msg) -> msg == err)
#endif

  -- describe "safeReadBackend" $ do
  --   it "parses valid backend strings" $ do
  --     safeReadBackend "postgres" `shouldBe` Just PG
  --     safeReadBackend "sqlite"   `shouldBe` Just SQLITE

  --   it "returns Nothing for invalid backend strings" $ do
  --     safeReadBackend "invalid" `shouldBe` Nothing
