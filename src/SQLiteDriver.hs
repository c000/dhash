{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SQLiteDriver where

import Import

import Database.SQLite.Simple

import FileWalker

withSQLite :: (MonadUnliftIO m, HasOptions a, MonadReader a m) => String -> (Connection -> m ()) -> m ()
withSQLite connectionString f = do
  tableName <- optionsTableName . getOptions <$> ask
  bracket (liftIO $ open connectionString) (liftIO . close) $ \conn -> do
    liftIO $ execute_ conn "PRAGMA journal_mode = MEMORY"
    liftIO $ execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> fromString tableName <> " (type, path, hash, size)"
    mask $ \restore -> do
      begin conn
      r <- restore (f conn) `onException` rollback conn
      commit conn
      return r
  where
    begin c    = liftIO $ execute_ c "BEGIN TRANSACTION"
    commit c   = liftIO $ execute_ c "COMMIT"
    rollback c = liftIO $ execute_ c "ROLLBACK"

insertResult :: (MonadUnliftIO m, HasOptions a, MonadReader a m)=> Connection -> ResultType -> m ()
insertResult conn r = do
  tableName <- optionsTableName . getOptions <$> ask
  liftIO $ execute conn ("insert into " <> fromString tableName <> " (type, path, hash, size) values (?, ?, ?, ?)") values
  where
    values = case r of
      File      p h s -> ("file"::Text, p, h , s)
      Directory p   s -> ("directory" , p, "", s)
