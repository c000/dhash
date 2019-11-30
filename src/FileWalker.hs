{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FileWalker where

import           Import
import           RIO.Directory
import           RIO.FilePath

import           System.IO.Error

import           Hash

type Callback m = ResultType -> m ()

walkAndHashFiles
  :: (MonadUnliftIO m, HasOptions a, HasLogFunc a, MonadReader a m)
  => [FilePath]
  -> Callback m
  -> m Integer
walkAndHashFiles files callback = do
  rec          <- optionsRecursive . getOptions <$> ask
  childrenSize <- forM files $ \f -> do
    checked <- (,) <$> doesFileExist f <*> doesDirectoryExist f
    case checked of
      (True , False) -> hashSingle f callback
      (False, True ) -> if rec
        then do
          logWarn
            $  displayShow f
            <> " is directory but recursive traverse are disabled"
          return 0
        else do
          eitherElems <- tryIO $ listDirectory f
          case eitherElems of
            Left err -> do
              logError . displayShow $ (f, err)
              return 0
            Right es -> do
              let children = map (f </>) es
              size <- walkAndHashFiles children callback
              callback $ Directory f size
              return size
      _ -> do
        logError $ displayShow f <> " is not a file or directory"
        return 0
  return $ sum childrenSize

hashSingle
  :: (MonadIO m, HasOptions a, HasLogFunc a, MonadReader a m)
  => FilePath
  -> Callback m
  -> m Integer
hashSingle f callback = do
  algo <- optionsHashAlgorithm . getOptions <$> ask
  r    <- liftIO $ hashFile algo f
  case r of
    Left ioe -> do
      logError . displayShow $ (f, ioeGetErrorType ioe)
      return 0
    Right cs -> do
      size <- getFileSize f
      callback $ File f cs size
      return size
