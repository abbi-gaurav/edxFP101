{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Types where

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
import           Database.SQLite.Simple           (Connection)
import           Database.SQLite.Simple.FromField

data Finished = FinishedTask deriving (Eq, Read, Show)
data InProgress = InProgressTask deriving (Eq, Read, Show)
data NotStarted = NotStartedTask deriving (Eq, Read, Show)

data TaskState = NotStarted
               | InProgress
               | Finished deriving (Enum, Read, Show)

type Complete f = f Identity
type Partial f = f Maybe

newtype TaskName = TaskName {getTName :: DT.Text} deriving (Eq, Show)
deriving instance FromField TaskName

newtype TaskDesc = TaskDesc {getTDesc :: DT.Text} deriving (Eq, Show)
deriving instance FromField TaskDesc

data Task f state = Task { tName        :: f TaskName
                         , tDescription :: f TaskDesc
                         , tState       :: f state
                         }

type CompletedTask = Task Identity Finished
deriving instance Eq CompletedTask
deriving instance Show CompletedTask

type InCompletePartialTask = Task Maybe InProgress
deriving instance Eq InCompletePartialTask
deriving instance Show InCompletePartialTask

type InCompleteTask = Task Identity InProgress
deriving instance Eq InCompleteTask
deriving instance Show InCompleteTask

type NotStartedPartialTask = Task Maybe NotStarted
deriving instance Eq NotStartedPartialTask
deriving instance Show NotStartedPartialTask

type NotStartedTask = Task Identity NotStarted
deriving instance Eq NotStartedTask
deriving instance Show NotStartedTask



newtype FieldName = FieldName {getFieldName :: DT.Text} deriving (Eq, Read, Show)

data ValidationError = InvalidField FieldName
                     | MissingField FieldName deriving (Eq, Read, Show)

type ValidationCheck t = t -> Maybe ValidationError

newtype Validated t = Validated {getVaidatedObj :: t}

newtype TaskID = TaskID { getTaskID :: DT.Text } deriving (Eq, Read, Show)

data TaskStoreError = NoSuchIdError TaskID
                    | UnexpectedError DT.Text
                    | Disconnected DT.Text deriving (Eq, Read, Show)

data WithID a where
  UUIDID :: UUID -> a -> WithID a
  Int64ID :: Int64 -> a -> WithID a

class Validatable t where
  isValid :: t -> Bool
  isValid = either (const False) (const True) . validate

  validate :: t -> Either [ValidationError] (Validated t)
  validate t = if null errors then Right (Validated t) else Left errors
    where
      checkResults :: [Maybe ValidationError]
      checkResults = [check t | check <- validationChecks]

      errors :: [ValidationError]
      errors = [fromJust e | e <- checkResults, isJust e]

  validationChecks :: [ValidationCheck t]

type FullySpecifiedTask = Task Identity
type PartialTask = Task Maybe

taskNameField :: FieldName
taskNameField = FieldName "name"

taskDescField :: FieldName
taskDescField = FieldName "description"

fsTaskName :: FullySpecifiedTask state -> DT.Text
fsTaskName = DT.strip . getTName . runIdentity . tName

fsTaskDesc :: FullySpecifiedTask state -> DT.Text
fsTaskDesc = DT.strip . getTDesc . runIdentity . tDescription

fsTaskState :: FullySpecifiedTask TaskState -> TaskState
fsTaskState = runIdentity . tState

instance Validatable (FullySpecifiedTask state) where
  validationChecks = [checkName, checkDescription]
    where
      checkName :: (FullySpecifiedTask state) -> Maybe ValidationError
      checkName t = if DT.null (fsTaskName t) then Just (InvalidField taskNameField) else Nothing

      checkDescription :: (FullySpecifiedTask state) -> Maybe ValidationError
      checkDescription t = if DT.null (fsTaskDesc t) then Just (InvalidField taskDescField) else Nothing

psTaskName :: PartialTask state -> Maybe DT.Text
psTaskName pt = (DT.strip . getTName) <$> tName pt

psTaskDesc :: PartialTask state -> Maybe DT.Text
psTaskDesc pt = (DT.strip . getTDesc) <$> tDescription pt

instance Validatable (PartialTask state) where
  validationChecks = [checkName, checkDescription]
    where
      notEmptyIfPresent :: FieldName -> DT.Text -> Maybe ValidationError
      notEmptyIfPresent fn v  = if DT.null v then Just (InvalidField fn) else Nothing

      checkName :: (PartialTask state) -> Maybe ValidationError
      --it becomes a partial fn until '.'
      checkName = maybe (Just (MissingField taskNameField)) (notEmptyIfPresent taskNameField) . psTaskName

      checkDescription :: (PartialTask state) -> Maybe ValidationError
      --it becomes a partial fn until '.'
      checkDescription = maybe (Just (MissingField taskDescField)) (notEmptyIfPresent taskDescField) . psTaskDesc
