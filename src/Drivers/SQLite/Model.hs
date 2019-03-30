{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Drivers.SQLite.Model where

import Import

import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Path = Path
  { path :: !String
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

- entity: Path
  constructors:
  - name: Path
    fields:
    - name: pathType
      dbName: type
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
