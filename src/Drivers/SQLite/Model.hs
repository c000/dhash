{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Drivers.SQLite.Model where

import Import
import RIO.Time
import Data.Maybe (fromJust)

import qualified Data.Aeson as A
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH
import Database.Groundhog.Sqlite ()

newtype ZonedTimeRFC3339 = ZTR ZonedTime
  deriving Show

instance PersistField ZonedTimeRFC3339 where
  persistName _ = "ZonedTimeRFC3339"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField ZonedTimeRFC3339 where
  toPrimitivePersistValue (ZTR t) = PersistText . either (error "ZonedTime encode error") id . decodeUtf8' . toStrictBytes $ A.encode t
  fromPrimitivePersistValue (PersistText t) = ZTR . fromJust . A.decode . fromStrictBytes . encodeUtf8 $ t
  fromPrimitivePersistValue _ = error "unexpected type of ZonedTimeRFC3339"

data ExecuteProperty = ExecuteProperty
  { basePath :: !String
  , time :: !ZonedTimeRFC3339
  }
deriving instance Show ExecuteProperty

data Path = Path
  { executePropertyId :: AutoKey ExecuteProperty
  , path :: !String
  , size :: !Int64
  , pathType :: !Text
  , hashId :: Maybe (AutoKey Hash)
  }
deriving instance Eq Path
deriving instance Show Path

data Hash = Hash
  { hash :: !String
  , hashType :: !Text
  }
deriving instance Eq Hash
deriving instance Show Hash

mkPersist (defaultCodegenConfig
  { namingStyle = lowerCaseSuffixNamingStyle
    { mkDbConstrAutoKeyName = \dName _ _ -> toUnderscore dName <> "_id"
    }
  }) [groundhog|
definitions:

- entity: ExecuteProperty

- entity: Path
  constructors:
  - name: Path
    fields:
    - name: pathType
      dbName: type
    - name: executePropertyId
      reference:
        onDelete: set null
        onUpdate: restrict
    - name: hashId
      reference:
        onDelete: set null
        onUpdate: restrict

- entity: Hash
  constructors:
  - name: Hash
    uniques:
    - name: hash_uix
      type: constraint
      fields: [hash, hashType]
|]
