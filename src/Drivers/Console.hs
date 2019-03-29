{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Drivers.Console where

import Import

import Data.Aeson
import Data.Text.Lazy.IO
import Text.Mustache

printResult :: (MonadUnliftIO m, HasLogFunc a, HasOptions a, MonadReader a m) => ResultType -> m ()
printResult r = do
  templateText <- optionsTemplate . getOptions <$> ask
  noNewline <- optionsNoNewline . getOptions <$> ask
  case compileMustacheText "console template" templateText of
    Left e -> logError $ displayShow e
    Right template -> do
      let printFunc = case noNewline of True  -> putStr
                                        False -> putStrLn
      liftIO $ printFunc $ renderMustache template (toJSON r)
