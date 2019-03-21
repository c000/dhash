{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ConsoleDriver where

import Import

import Data.Aeson
import Data.Text.Lazy.IO
import Text.Mustache

printResult :: (MonadUnliftIO m, HasLogFunc a, HasOptions a, MonadReader a m) => ResultType -> m ()
printResult r = do
  templateText <- optionsTemplate . getOptions <$> ask
  case compileMustacheText "console template" templateText of
    Left e -> logError $ displayShow e
    Right template -> do
      liftIO $ putStrLn $ renderMustache template (toJSON r)
