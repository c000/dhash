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

import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data ExecuteProperty = ExecuteProperty
  { basePath :: !String
  , time :: !UTCTime
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
