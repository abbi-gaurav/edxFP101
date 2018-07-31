module Config where

import           Data.Default             (def)
import qualified Database.HDBC            as DB
import qualified Database.HDBC.Sqlite3    as DB
import           Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import           System.Environment       (lookupEnv)
import           Web.Scotty

data Config = Config {environment :: Environment}

data Environment = Dev | Prod | Test deriving (Eq, Read, Show)

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "APP_ENV"
  let e = case m of
        Nothing -> Dev
        Just s  -> read s
  return e
{--
getEnvironment = fmap
  (maybe Dev read)
  (lookupEnv "APP_ENV")
--}

getDB :: Environment -> IO DB.Connection
getDB env = DB.connectSqlite3 ((show env) ++ ".db")

getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return def {settings = s,
             verbose = 0}

getSettings :: Environment -> IO Settings
getSettings e = do
  let s = defaultSettings
  m <- getPort
  let s' = case m of
        Nothing -> s
        Just p  -> setPort p s
  return s'

getPort :: IO (Maybe Int)
getPort = do
  m <- lookupEnv "PORT"
  let p = case m of
        Nothing -> Nothing
        Just s  -> Just (read s)
  return p
