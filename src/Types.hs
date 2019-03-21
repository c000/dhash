{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO
import RIO.Process
import Prelude (tail)

import Hash.Algorithms
import qualified Data.Aeson.TH as A

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsHashAlgorithm :: !HashAlgorithm
  , optionsFiles :: [FilePath]
  , optionsRecursive :: !Bool
  , optionsDSN :: Maybe String
  , optionsTableName :: !String
  , optionsTemplate :: !Text
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

type Size = Integer
type HashValue = String

data ResultType
  = File
    { _path :: FilePath
    , _hash :: HashValue
    , _size :: Size
    }
  | Directory
    { _path :: FilePath
    , _size :: Size
    }
  deriving (Eq, Show)

A.deriveJSON
  ( A.defaultOptions
    { A.fieldLabelModifier = tail
    , A.sumEncoding = A.TaggedObject
      { A.tagFieldName = "type"
      , A.contentsFieldName = undefined
      }
    }
  )
  ''ResultType
