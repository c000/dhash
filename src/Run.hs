{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

import System.IO.Error

import System.IO.Streams as S
import Crypto.Hash

hashFile :: (HashAlgorithm a, MonadUnliftIO m) => FilePath -> m (Either IOError (Digest a))
hashFile path = tryIO . liftIO $ do
  h <- S.withFileAsInput path $ S.fold hashUpdate hashInit
  return $ hashFinalize h

run :: RIO App ()
run = do
  files <- optionsFiles . appOptions <$> ask
  forM_ files $ \f -> do
    r <- hashFile f
    case r of
      Left ioe -> logError . displayShow $ (f, ioeGetErrorType ioe, ioeGetFileName ioe)
      Right cs -> logInfo $ displayShow (f, cs :: Digest MD5)
