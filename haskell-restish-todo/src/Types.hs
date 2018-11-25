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

import           Data.Functor.Identity
import           Data.Text

data Finished = FinishedTask deriving (Eq, Read, Show)
data InProgress = InProgressTask deriving (Eq, Read, Show)
data NotStarted = NotStartedTask deriving (Eq, Read, Show)

data TaskState = NotStarted
               | InProgress
               | Finished deriving (Enum, Read, Show)

type Complete f = f Identity
type Partial f = f Maybe

newtype TaskName = TaskName {getTName :: Text} deriving (Eq, Show)
newtype TaskDesc = TaskDesc {getTDesc :: Text} deriving (Eq, Show)

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



newtype FieldName = FieldName {getFieldName :: Text} deriving (Eq, Read, Show)

data ValidationError = InvalidField FieldName
                     | MissingField FieldName deriving (Eq, Read, Show)

data Validated t = Validated t

class Validatable t where
  validate :: t -> Either ValidationError (Validated t)
