{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SQLiteDriver where

import Import

import Database.SQLite.Simple

import FileWalker

withSQLite :: MonadUnliftIO m => String -> (Connection -> m ()) -> m ()
withSQLite connectionString f = do
  bracket (liftIO $ open connectionString) (liftIO . close) $ \conn -> do
    liftIO $ execute_ conn "PRAGMA journal_mode = MEMORY"
    liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS files (type, path, hash, size)"
    mask $ \restore -> do
      begin conn
      r <- restore (f conn) `onException` rollback conn
      commit conn
      return r
  where
    begin c    = liftIO $ execute_ c "BEGIN TRANSACTION"
    commit c   = liftIO $ execute_ c "COMMIT"
    rollback c = liftIO $ execute_ c "ROLLBACK"

insertResult :: MonadUnliftIO m => Connection -> ResultType -> m ()
insertResult conn r = liftIO $ do
  case r of
    File      p h s -> execute conn "insert into files (type, path, hash, size) values (?, ?, ?, ?)"
                                    ("file"::Text, p, h, s)
    Directory p   s -> execute conn "insert into files (type, path, hash, size) values (?, ?, ?, ?)"
                                    ("directory"::Text, p, ""::Text, s)
