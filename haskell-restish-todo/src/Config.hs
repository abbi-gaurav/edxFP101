{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Config where

import           Data.Aeson
import           Data.Bifunctor        (first)
import qualified Data.ByteString.Lazy  as DBL
import           Data.Functor.Identity

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

type CompleteAppConfig = FancyAppConfig Identity
type PartialAppConfig = FancyAppConfig Maybe

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

instance FromJSON PartialAppConfig where
  parseJSON = withObject "cfg" parseObj
    where
      parseObj obj = obj .: "host"
                     >>= \host -> obj .: "port"
                     >>= \port -> pure $ FancyAppConfig {facHost = host, facPort = port}

class (FromJSON cfg) => FromJSONFile cfg where
  fromJSONFile :: FilePath -> IO (Either ConfigurationError cfg)

instance FromJSONFile PartialAppConfig where
  fromJSONFile path = decodeAndTransformError <$> DBL.readFile path
    where
      decodeAndTransformError :: DBL.ByteString -> Either ConfigurationError PartialAppConfig
      decodeAndTransformError = first ConfigParseError . eitherDecode
