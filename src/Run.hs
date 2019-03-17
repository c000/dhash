{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

import FileWalker

run :: RIO App ()
run = do
  files <- optionsFiles . appOptions <$> ask
  _ <- walkAndHashFiles files $ \hash -> do
    logInfo . displayShow $ hash
  return ()
