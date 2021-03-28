{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dhash.Sqlite.SchemaSpec
  ( spec
  )
where

import           Import
import           Test.Hspec

import           Database.SQLite.Simple
import           Database.Beam
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate (migrationBackend)
import           Dhash.Sqlite.Schema

spec :: Spec
spec = describe "Migration" $ do
  instructions <- runIO $ newIORef []
  c <- runIO $ open ":memory:"
  runIO $ setTrace c (Just (\x -> modifyIORef instructions (x :)))

  it "can migrate" $
    ( do
        execute_ c "PRAGMA joirnal_mode = MEMORY"

        runBeamSqlite c $ autoMigrate migrationBackend checkedDhashDb
        execute_ c "CREATE UNIQUE INDEX IF NOT EXISTS hash_uix on hash (hash, hash_type)"

        r <- runBeamSqlite c $ verifySchema migrationBackend checkedDhashDb
        return (show r)
    ) `shouldReturn` "VerificationSucceeded"

  it "can insert to hash" $
    ( runBeamSqlite c $ do
        runInsert $ insert (dbHash dhashDb) $ insertExpressions
          [ Hash default_ "hash" "type"
          ]
    ) `shouldReturn` ()

  it "error on duplicate hash" $
    ( runBeamSqlite c $ do
        runInsert $ insert (dbHash dhashDb) $ insertExpressions
          [ Hash default_ "hash" "type"
          ]
    ) `shouldThrow` (\e -> sqlErrorDetails (e :: SQLError)
        == "UNIQUE constraint failed: hash.hash, hash.hash_type")
