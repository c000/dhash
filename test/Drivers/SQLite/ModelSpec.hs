{-# LANGUAGE NoImplicitPrelude #-}
module Drivers.SQLite.ModelSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances.Time ()

import qualified Data.Aeson as A
import Database.Groundhog.Sqlite
import Drivers.SQLite.Model

spec :: Spec
spec = do
  describe "ZonedTimeRFC3339" $ do
    prop "convert" $ \p -> \s -> monadicIO $ do
      run $ withSqliteConn ":memory:" $ runDbConn $ do
        runMigrationUnsafe $ do
          migrate (undefined :: ExecuteProperty)
        i <- insert $ ExecuteProperty s (ZTR p)
        Just (ExecuteProperty s' (ZTR p')) <- get i
        return $ A.encode (s, p) `shouldBe` A.encode (s', p')
