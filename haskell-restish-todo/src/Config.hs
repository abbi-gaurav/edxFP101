{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where

import           Data.Aeson
import           Data.Aeson.Types      (parseEither)
import           Data.Bifunctor        (bimap, first, second)
import qualified Data.ByteString.Lazy  as DBL
import           Data.Functor.Identity
import qualified Data.Text.IO          as DTI
import           GHC.Generics
import           Text.Parsec.Error     (ParseError)
import           Text.Toml             (parseTomlDoc)

data AppConfig = AppConfig{
  acHost :: String,
  acPort :: Integer
  }

-- Todo : improve with newtype? use existing types
type Host = String
type Port = Integer

data FancyAppConfig f = FancyAppConfig {
  facHost :: f Host,
  facPort :: f Port
  }


data ConfigurationError = ConfigParseError String
                        | TOMLParseError ParseError
{-
{
    "facHost" : "ssss",
    "facPort" : 1234
}

 cfg = ( eitherDecode <$> DBL.readFile ("/tmp/test.json" :: FilePath) :: IO (Either String (FancyAppConfig Identity)))
-}
type CompleteAppConfig = FancyAppConfig Identity
deriving instance Generic CompleteAppConfig
deriving instance Eq CompleteAppConfig
deriving instance Show CompleteAppConfig
deriving instance FromJSON CompleteAppConfig

type PartialAppConfig = FancyAppConfig Maybe
deriving instance Generic PartialAppConfig
deriving instance Eq PartialAppConfig
deriving instance Show PartialAppConfig
deriving instance FromJSON PartialAppConfig

class HasDefaultValue a where
  defaultValue :: a

defaultHost :: Host
defaultHost = "localhost"

defaultPort :: Port
defaultPort = 5000

instance HasDefaultValue (FancyAppConfig Identity) where
  defaultValue = FancyAppConfig (Identity defaultHost) (Identity defaultPort)

instance HasDefaultValue (FancyAppConfig Maybe) where
  defaultValue = FancyAppConfig (Just defaultHost) (Just defaultPort)

class (FromJSON cfg) => FromJSONFile cfg where
  fromJSONFile :: FilePath -> IO (Either ConfigurationError cfg)

instance FromJSONFile PartialAppConfig where
  fromJSONFile path = decodeAndTransformError <$> DBL.readFile path
    where
      decodeAndTransformError :: DBL.ByteString -> Either ConfigurationError PartialAppConfig
      decodeAndTransformError = first ConfigParseError . eitherDecode

class (FromJSONFile cfg) => FromTOMLFile cfg where
  fromTOMLFile :: FilePath -> IO (Either ConfigurationError cfg)

instance FromTOMLFile PartialAppConfig where
  fromTOMLFile path = DTI.readFile path
                      >>= pure . first TOMLParseError . parseTomlDoc ""
                      >>= pure . second (parseEither parseJSON . toJSON)
                      >>= \v -> pure $ case v of
                                         Right (Right cfg) -> Right cfg
                                         Right (Left err)  -> Left (ConfigParseError err)
                                         Left err          -> Left err
