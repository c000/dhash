{-# LANGUAGE NoImplicitPrelude #-}
module Hash where

import Import

import System.IO.Error

import System.IO.Streams as S
import Crypto.Hash
import qualified Hash.Algorithms as A

type StrictDigest a = Digest a -> Digest a

hashFile :: MonadUnliftIO m => A.HashAlgorithm -> FilePath -> m (Either IOError String)
hashFile algo path = tryIO . liftIO $ do
  S.withFileAsInput path $ strictHasher
  where
    hasher :: HashAlgorithm a => InputStream ByteString -> IO (Digest a)
    hasher = fmap hashFinalize . S.fold hashUpdate hashInit
    strictHasher = case algo of
      A.SHA512 -> fmap (show . (id :: StrictDigest SHA512)) . hasher
      A.SHA384 -> fmap (show . (id :: StrictDigest SHA384)) . hasher
      A.SHA256 -> fmap (show . (id :: StrictDigest SHA256)) . hasher
      A.SHA224 -> fmap (show . (id :: StrictDigest SHA224)) . hasher
      A.SHA1   -> fmap (show . (id :: StrictDigest SHA1)  ) . hasher
      A.MD5    -> fmap (show . (id :: StrictDigest MD5)   ) . hasher
