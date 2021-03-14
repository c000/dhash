{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Dhash.Console where

import           Import

import           Data.Aeson
import           Data.Text.Lazy.IO
import           Text.Microstache

printResult
  :: (MonadUnliftIO m, HasLogFunc a, HasOptions a, MonadReader a m)
  => ResultType
  -> m ()
printResult r = do
  templateText <- optionsTemplate . getOptions <$> ask
  noNewline    <- optionsNoNewline . getOptions <$> ask
  case compileMustacheText "console template" templateText of
    Left  e        -> logError $ displayShow e
    Right template -> do
      let printFunc = if noNewline then putStr else putStrLn
      liftIO $ printFunc $ renderMustache template (toJSON r)
