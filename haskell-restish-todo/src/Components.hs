{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Components where

import           Config
import           Control.Exception                (Exception,
                                                   SomeException (..), catch,
                                                   throw)
import           Data.Either                      (isRight)
import           Data.Functor.Identity            (Identity (..))
import           Data.Int                         (Int64)
import           Data.Maybe                       (fromJust, isJust)
import qualified Data.Text                        as DT
import           Data.UUID                        (UUID, fromText, toText)
import           Data.UUID.V4                     (nextRandom)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           SQLLite
import           Types

-----------------
-- Components --
-----------------

class Component c where
  start :: c -> IO ()
  stop :: c -> IO ()

class Component c => TaskStore c where
  persistTask :: ToField state => c -> Validated (FullySpecifiedTask state) -> IO (Either TaskStoreError (WithID (FullySpecifiedTask state)))
  completeTask :: c -> TaskID -> IO (Either TaskStoreError CompletedTask)
  getTask :: c -> TaskID -> IO(Either TaskStoreError (FullySpecifiedTask state))
  updateTask :: c -> TaskID -> PartialTask state -> IO (Either TaskStoreError (FullySpecifiedTask state))
  deletTask :: c -> TaskID -> IO(Either TaskStoreError (FullySpecifiedTask state))


class Component c => Constructable c cfg err where
  construct :: cfg -> IO (Either err c)

data SQLliteTaskStore = SQLliteTaskStore
  {
    stsConfig :: CompleteTaskStoreConfig
  , stsConn   :: Maybe Connection
  }

instance Component SQLliteTaskStore where
  start = undefined
  stop = undefined

disconnectionError :: IO (Either TaskStoreError a)
disconnectionError = pure $ Left $ Disconnected "Store is disconnected"

makeGenericInsertError :: SomeException -> IO (Either TaskStoreError a)
makeGenericInsertError = pure . Left . UnexpectedError . ("Insert command failed " <>) . DT.pack . show

saveAndReturnTask :: ToField state => Connection -> WithID (FullySpecifiedTask state) -> IO (Either TaskStoreError (WithID (FullySpecifiedTask state)))
saveAndReturnTask c t = catch doInsert makeGenericInsertError
  where
    doInsert = execute c "INSERT INTO tasks (uuid, name, desc, state) VALUES (?,?,?,?)" t >> pure (Right t)

instance TaskStore SQLliteTaskStore where
  persistTask :: ToField state => SQLliteTaskStore -> Validated (FullySpecifiedTask state) -> IO (Either TaskStoreError (WithID (FullySpecifiedTask state)))
  persistTask store (Validated newTask) = maybe disconnectionError _handler $ stsConn store
    where
      _handler conn = (flip UUIDID newTask <$> nextRandom) >>= saveAndReturnTask conn

  getTask = undefined
  updateTask = undefined
  deletTask = undefined
  completeTask = undefined
