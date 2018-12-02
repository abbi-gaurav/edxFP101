{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

module SQLLite where

import           Control.Exception                (Exception,
                                                   SomeException (..), catch,
                                                   throw)
import           Data.Functor.Identity            (Identity (..))
import qualified Data.Text                        as DT
import           Data.UUID                        (UUID, fromText, toText)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           Types

instance ToRow a => ToRow (Validated a) where
  toRow :: Validated a -> [SQLData]
  toRow = toRow . getVaidatedObj

instance ToField UUID where
  toField :: UUID -> SQLData
  toField = SQLText . toText

instance ToField TaskState where
  toField :: TaskState -> SQLData
  toField = SQLText . DT.pack . show

instance ToRow a => ToRow (WithID a) where
  toRow :: WithID a -> [SQLData]
  toRow (UUIDID id_ obj)  = [toField id_] <> toRow obj
  toRow (Int64ID id_ obj) = [toField id_] <> toRow obj

instance ToField a => ToField (Identity a) where
  toField = toField

instance ToField state => ToRow (FullySpecifiedTask state) where
  toRow t = toRow (fsTaskName t, fsTaskDesc t, tState t)

instance FromRow a => FromRow (WithID a) where
  fromRow = field
            >>= \idSQLData -> fromRow
            >>= chooseCtor idSQLData
    where
      chooseCtor sqlData = case sqlData of
                             (SQLText txt) -> \obj -> case fromText txt of
                                                        Nothing -> throw (ConversionFailed (show sqlData) "Text" "Invalid UUID failed fromText conversion")
                                                        Just uuid -> pure $ UUIDID uuid obj
                             (SQLInteger int) -> \obj -> pure $ Int64ID (fromIntegral int) obj
                             _ -> throw (ConversionFailed (show sqlData) "???" "Unrecognized contents in ID field (no valid WithID GADT constructor)")

instance FromRow a => FromRow (Identity a) where
  fromRow = fromRow

instance FromField a => FromField (Identity a) where
  fromField = fromField

instance FromField TaskState where
  fromField f = case fieldData f of
                  SQLText txt -> pure $ read $ DT.unpack txt
                  fd -> returnError ConversionFailed f "unexpected TaskState field type"

instance (FromField state) => FromRow (FullySpecifiedTask state) where
  fromRow = Task <$> field <*> field <*> field

instance (FromField state) => FromRow (PartialTask state) where
    fromRow = Task <$> field <*> field <*> field
