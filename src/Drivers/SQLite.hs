{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Drivers.SQLite
  ( withSQLite
  , insertResult
  ) where

import Import
import Control.Monad.Trans.Control

import qualified Database.SQLite3 as S
import Database.Groundhog.Core (runDbConn')
import Database.Groundhog.Sqlite hiding (runDbConn)

import qualified Drivers.SQLite.Model as M

withSQLite :: (MonadBaseControl IO m, MonadUnliftIO m) => Text -> (Sqlite -> m ()) -> m ()
withSQLite connectionString f = do
  bracket (openDB connectionString) closeDB $ \conn -> do
    mask $ \restore -> do
      begin conn
      flip runDbConn' conn $ runMigration $ do
        migrate (undefined :: M.Path)
        migrate (undefined :: M.Hash)
      restore (f conn) `onException` rollback conn
      commit conn
  where
    begin c    = execDB c "BEGIN"
    commit c   = execDB c "COMMIT"
    rollback c = execDB c "ROLLBACK"

insertResult :: (MonadBaseControl IO m, MonadUnliftIO m, HasLogFunc a, HasOptions a, MonadReader a m) => Sqlite -> ResultType -> m ()
insertResult conn r = do
  algo <- optionsHashAlgorithm . getOptions <$> ask
  case r of
    Directory p   s -> do
      let v = M.Path p (fromIntegral s) "dir" Nothing
      logDebug $ "insert " <> displayShow v
      insert_' v
    File      p h s -> do
      i <- upsert' $ M.Hash h (tshow algo)
      insert_' $ M.Path p (fromIntegral s) "file" (Just i)
  where
    insert_' v = do
      logDebug $ "insert " <> displayShow v
      runDbConn' (insert_ v) conn
    upsert' v = do
      logDebug $ "upsert " <> displayShow v
      i <- runDbConn' (insertByAll v) conn
      return $ either id id i

openDB :: MonadIO m => Text -> m Sqlite
openDB s = Sqlite <$> mconn <*> newIORef mempty
  where
    mconn = do
      conn <- liftIO $ S.open s
      liftIO $ S.exec conn "PRAGMA foregin_keys = ON"
      liftIO $ S.exec conn "PRAGMA journal_mode = MEMORY"
      return conn

closeDB :: MonadIO m => Sqlite -> m ()
closeDB (Sqlite conn caches) = do
  cs <- readIORef caches
  mapM_ (liftIO . S.finalize) $ toList cs
  liftIO $ S.close conn

execDB :: MonadIO m => Sqlite -> Text -> m ()
execDB (Sqlite conn _) t = do
  liftIO $ S.exec conn t
