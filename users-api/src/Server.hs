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

import           Control.Monad.Except
import           Data.Aeson.Compat
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                       ()
import           Prelude.Compat
import           Servant
import           Servant.HTML.Lucid
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

data User = User { name            :: String,
                   age             :: Int,
                   email           :: String,
                   registrationDay :: Day
                 } deriving (Eq, Show, Generic)
instance ToJSON User

abc :: User
abc = User "abc" 22 "x@y.z" (fromGregorian 1984 2 1)

def ::User
def = User "def" 23 "d@y.z" (fromGregorian 1980 2 1)

users1 :: [User]
users1 = [ abc  , def ]

type UsersAPI1 = "users" :> Get '[JSON] [User]

type UsersAPI2 = "users" :> Get '[JSON] [User]
                 :<|> "abc" :> Get '[JSON] User
                 :<|> "def" :> Get '[JSON] User

server1 :: Server UsersAPI1
server1 = return users1

server2 :: Server UsersAPI2
server2 = return users1
          :<|> return abc
          :<|> return def

userAPI :: Proxy UsersAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

type PersonsAPI = "persons" :> Get '[JSON, HTML] [Person]
data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- for the JSON instance

instance ToJSON Person

instance ToHtml Person where
  toHtml person = tr_ $ do
    td_ (toHtml $ firstName person)
    td_ (toHtml $ lastName person)
  toHtmlRaw = toHtml

instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "firstname"
      th_ "lastname"
    foldMap toHtml persons
  toHtmlRaw = toHtml

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonsAPI
personAPI = Proxy

server4 :: Server PersonsAPI
server4 = return people

app2 :: Application
app2 = serve personAPI server4

startApp :: IO()
startApp = run 8081 app2
