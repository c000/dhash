{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process
import Hash.Algorithms

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsHashAlgorithm :: !HashAlgorithm
  , optionsFiles :: [FilePath]
  }

class HasOptions a where
  getOptions :: a -> Options

instance HasOptions Options where
  getOptions = id

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasOptions App where
  getOptions = appOptions

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
