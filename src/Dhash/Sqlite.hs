{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Dhash.Sqlite
  ( insertResult
  , withSqlite
  )
where

import           Import
import           RIO.Directory
import           RIO.Text
import           RIO.Time
import           Control.Monad.Trans.Control

import           Database.SQLite.Simple
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Simple ( autoMigrate
                                              , verifySchema
                                              , VerificationResult(..)
                                              )
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate

import           Dhash.Sqlite.Schema

data DriverContext = DC Connection (PrimaryKey ExecutePropertyT Identity)

insertResult
  :: ( MonadBaseControl IO m
     , MonadUnliftIO m
     , HasLogFunc a
     , HasOptions a
     , MonadReader a m
     )
  => DriverContext
  -> ResultType
  -> m ()
insertResult (DC conn ep) r = do
  algo <- optionsHashAlgorithm . getOptions <$> ask

  case r of
    v@(Directory p s) -> do
      logDebug $ "insert " <> displayShow v

      liftIO . runBeamSqlite conn . runInsert $
        insert (dbPath dhashDb) $ insertExpressions
          [ Path (val_ ep)
                 default_
                 (val_ . pack $ p)
                 (fromIntegral s)
                 "dir"
                 nothing_
          ]
    v@(File p h s) -> do
      logDebug $ "insert " <> displayShow v

      hashPk <- upsertHash (pack h) (tshow algo)

      liftIO . runBeamSqlite conn $ do
        runInsert . insert (dbPath dhashDb) $ insertExpressions
          [ Path (val_ ep)
                 default_
                 (val_ . pack $ p)
                 (fromIntegral s)
                 "file"
                 (just_ . val_ $ hashPk)
          ]
 where
  upsertHash h algo = do
    hashRow <- liftIO . runBeamSqlite conn . runSelectReturningOne . select $ do 
      rows <- all_ (dbHash dhashDb)
      guard_ (hHashType rows ==. val_ algo)
      guard_ (hHash rows ==. val_ h)
      return rows

    case hashRow of
      Just row -> return $ primaryKey row
      Nothing -> liftIO $ do
        runBeamSqlite conn . runInsert . insert (dbHash dhashDb) $ insertExpressions
          [ Hash default_
                 (val_ h)
                 (val_ algo)
          ]
        rowid <- lastInsertRowId conn
        return $ HashId . SqlSerial $ rowid

withSqlite
  :: (MonadBaseControl IO m, MonadUnliftIO m)
  => Text
  -> (DriverContext -> m ())
  -> m ()
withSqlite connectionString f = do
  -- Insert execution property
  cwd <- getCurrentDirectory
  t   <- getZonedTime

  bracket openDB closeDB $ \c ->
    mask $ \restore -> do
      liftIO $ do
        execute_ c "PRAGMA joirnal_mode = MEMORY"
        migrate c

      row <- liftIO $ do
        execute_ c "BEGIN"

        [row] <- runBeamSqlite c $ do
          runInsertReturningList $ insertReturning executeProperty $ insertExpressions
            [ ExecuteProperty default_ (val_ . pack $ cwd) (val_ . formatTime' $ t)
            ]
        return row

      restore (f $ DC c (primaryKey row))
        `onException` liftIO (execute_ c "ROLLBACK")
      liftIO $ execute_ c "COMMIT"
 where
  executeProperty = dbExecuteProperty dhashDb
  formatTime' = pack . formatTime defaultTimeLocale "%FT%T.%q"
  openDB = liftIO . open $ unpack connectionString
  closeDB = liftIO . close

  migrate c = do
    r <- runBeamSqlite c $ verifySchema migrationBackend checkedDhashDb
    case r of
      VerificationSucceeded -> return ()
      VerificationFailed _ -> do
        runBeamSqlite c $ autoMigrate migrationBackend checkedDhashDb
        execute_ c "CREATE UNIQUE INDEX IF NOT EXISTS hash_uix on hash (hash, hash_type)"
