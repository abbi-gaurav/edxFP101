{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

data User = User
  { name              :: String
  , age               :: Int
  , email             :: String
  , registration_date :: Day
  } deriving (Eq, Generic, Show)

instance ToJSON User

type UserAPI1
   = "users" :> Get '[ JSON] [User] :<|> "albert" :> Get '[ JSON] User :<|> "issac" :> Get '[ JSON] User

users1 :: [User]
users1 = [issac, albert]

issac :: User
issac = User "Issac Nwton" 372 "issac.newton@co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage
  { msg :: String
  } deriving (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving (Generic)

instance ToJSON ClientInfo

instance FromJSON ClientInfo

data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient cInfo = Email from' to' subject' body'
  where
    from' = "a@b.com"
    to' = clientEmail cInfo
    subject' = "Hey " ++ clientName cInfo ++ " we miss you"
    body' = "have you checked " ++ intercalate ", " (clientInterestedIn cInfo)

type API
   = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[ JSON] Position :<|> "hello" :> QueryParam "name" String :> Get '[ JSON] HelloMessage :<|> "marketing" :> ReqBody '[ JSON] ClientInfo :> Post '[ JSON] Email

server2 :: Server API
server2 = position :<|> hello :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)
    hello :: Maybe String -> Handler HelloMessage
    hello mname =
      return . HelloMessage $
      case mname of
        Nothing -> "hello anonymous"
        Just n  -> "hello " ++ n
    marketing :: ClientInfo -> Handler Email
    marketing clientInfo = return (emailForClient clientInfo)

api :: Proxy API
api = Proxy

app2 :: Application
app2 = serve api server2

server1 :: Server UserAPI1
server1 = return users1 :<|> return albert :<|> return issac

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

startServer :: IO ()
startServer = run 8081 app2
