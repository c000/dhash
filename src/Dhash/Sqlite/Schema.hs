{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Dhash.Sqlite.Schema where

import Import

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Sqlite

-- ExecuteProperty
data ExecutePropertyT f = ExecuteProperty
  { epId :: C f (SqlSerial Int64)
  , epBasePath :: C f Text
  , epTime :: C f Text
  } deriving Generic
instance Beamable ExecutePropertyT

instance Table ExecutePropertyT where
  data PrimaryKey ExecutePropertyT f =
    ExecutePropertyId (C f (SqlSerial Int64)) deriving Generic
  primaryKey = ExecutePropertyId . epId
instance Beamable (PrimaryKey ExecutePropertyT)

type ExecuteProperty = ExecutePropertyT Identity
deriving instance Show ExecuteProperty
deriving instance Show (PrimaryKey ExecutePropertyT Identity)

-- Hash
data HashT f = Hash
  { hId :: C f (SqlSerial Int64)
  , hHash :: C f Text
  , hHashType :: C f Text
  } deriving Generic
instance Beamable HashT

instance Table HashT where
  data PrimaryKey HashT f =
    HashId (C f (SqlSerial Int64)) deriving Generic
  primaryKey = HashId . hId
instance Beamable (PrimaryKey HashT)

type Hash = HashT Identity
deriving instance Show Hash
deriving instance Show (PrimaryKey HashT Identity)
deriving instance Show (PrimaryKey HashT (Nullable Identity))

-- Path
data PathT f = Path
  { pExecuteProperty :: PrimaryKey ExecutePropertyT f
  , pId :: C f (SqlSerial Int64)
  , pPath :: C f Text
  , pSize :: C f Int64
  , pPathType :: C f Text
  , pHash :: PrimaryKey HashT (Nullable f)
  } deriving Generic
instance Beamable PathT

instance Table PathT where
  data PrimaryKey PathT f =
    PathId (C f (SqlSerial Int64)) deriving Generic
  primaryKey = PathId . pId
instance Beamable (PrimaryKey PathT)

type Path = PathT Identity
deriving instance Show Path
deriving instance Show (PrimaryKey PathT Identity)

-- DB
data DhashDb entity = DhashDb
  { dbExecuteProperty :: entity (TableEntity ExecutePropertyT)
  , dbHash :: entity (TableEntity HashT)
  , dbPath :: entity (TableEntity PathT)
  } deriving Generic
instance Database be DhashDb

checkedDhashDb :: CheckedDatabaseSettings Sqlite DhashDb
checkedDhashDb = defaultMigratableDbSettings

dhashDb :: DatabaseSettings Sqlite DhashDb
dhashDb = unCheckDatabase checkedDhashDb
