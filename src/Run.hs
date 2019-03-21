{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

import FileWalker
import SQLiteDriver
import ConsoleDriver

run :: RIO App ()
run = do
  files <- optionsFiles . appOptions <$> ask
  dsn <- optionsDSN . appOptions <$> ask
  case dsn of
    Nothing -> do
      _ <- walkAndHashFiles files $ \hash -> do
        printResult hash
      return ()
    Just s -> do
      withSQLite s $ \c -> do
        _ <- walkAndHashFiles files $ \hash -> do
          insertResult c hash
          logDebug . displayShow $ hash
        return ()
