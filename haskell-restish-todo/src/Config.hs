{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where

import           Control.Applicative       ((<|>))
import           Control.Exception         (Exception, throw, try)
import           Control.Monad             (join, when)
import           Data.Aeson
import           Data.Aeson.Types          (parseEither)
import           Data.Bifunctor            (bimap, first, second)
import qualified Data.ByteString.Lazy      as DBL
import           Data.Functor.Identity
import           Data.Maybe                (fromMaybe)
import           Data.Text
import qualified Data.Text.IO              as DTI
import qualified Filesystem.Path           as FP
import qualified Filesystem.Path.CurrentOS as FCOS
import           GHC.Generics
import           Text.Parsec.Error         (ParseError)
import           Text.Read                 (readMaybe)
import           Text.Toml                 (parseTomlDoc)

data AppConfig = AppConfig{
  acHost :: String,
  acPort :: Integer
  }

-- Todo : improve with newtype? use existing types
type Host = String
type Port = Integer

newtype ProcessEnvironment = ProcessEnvironment {getProcessEnv :: [(String,String)]} deriving Eq

data FancyAppConfig f = FancyAppConfig {
  facHost :: f Host,
  facPort :: f Port
  }


data ConfigurationError = ConfigParseError String
                        | TOMLParseError ParseError
                        | InvalidPath FP.FilePath String
deriving instance Show ConfigurationError
deriving instance Exception ConfigurationError
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

class FromENV cfg where
  fromEnv :: ProcessEnvironment -> cfg

instance FromENV PartialAppConfig where
  fromEnv pEnv = FancyAppConfig { facHost = prop "host",
                                  facPort = join $ fmap readMaybe $ prop "port"
                                }
                 where
                   env :: [(String,String)]
                   env = getProcessEnv pEnv

                   prop :: String -> Maybe String
                   prop = flip lookup env

class MergeOverridable a where
  mergeOverride :: a -> a -> a

instance MergeOverridable PartialAppConfig where
  mergeOverride a b = FancyAppConfig { facHost = resolveMaybes facHost
                                     , facPort = resolveMaybes facPort
                                     }
                      where
                        resolveMaybes :: (PartialAppConfig -> Maybe a) -> Maybe a
                        resolveMaybes fn = (fn b) <|> (fn a)

mergeInPartial :: CompleteAppConfig -> PartialAppConfig -> CompleteAppConfig
mergeInPartial c p = FancyAppConfig { facHost = fromMaybe (facHost c) (Identity <$> facHost p),
                                      facPort = fromMaybe (facPort c) (Identity <$> facPort p)
                                    }

rightOrThrow :: Exception a => Either a b -> IO b
rightOrThrow e = case e of
                   (Left err) -> throw err
                   (Right v)  -> return v

buildConfigWithDefault :: CompleteAppConfig -> [PartialAppConfig] -> CompleteAppConfig
buildConfigWithDefault orig partials = orig `mergeInPartial` combinedPartials
  where
    combinedPartials :: PartialAppConfig
    combinedPartials = Prelude.foldl mergeOverride (defaultValue :: PartialAppConfig) partials

makeAppConfig :: ProcessEnvironment -> FP.FilePath -> IO (Either ConfigurationError CompleteAppConfig)
makeAppConfig env path = try generateConfig
  where
    extension :: Maybe Text
    extension = FP.extension path

    isJsonExtension :: String -> Bool
    isJsonExtension = (== "json")

    isTOMLExtension = (== "toml")

    isJSONFile :: Bool
    isJSONFile = maybe False id $ (isJsonExtension . unpack) <$> extension

    isTOMLFile :: Bool
    isTOMLFile = maybe False id $ (isTOMLExtension . unpack) <$> extension

    pathExtensionIsInvalid :: Bool
    pathExtensionIsInvalid = not $ isJSONFile || isTOMLFile

    pathInvalidExtensionErr :: ConfigurationError
    pathInvalidExtensionErr = InvalidPath path "Path is invalid (must be either .json or .toml patg)"

    envCfg :: PartialAppConfig
    envCfg = (fromEnv env :: PartialAppConfig)

    getFileConfig :: IO (Either ConfigurationError PartialAppConfig)
    getFileConfig = case isJSONFile of
                      True  -> fromJSONFile (FCOS.encodeString path)
                      False -> fromJSONFile (FCOS.encodeString path)

    generateConfig :: IO CompleteAppConfig
    generateConfig = when pathExtensionIsInvalid (throw pathInvalidExtensionErr)
                     >> getFileConfig
                     >>= rightOrThrow
                     >>= \fileCfg -> pure (buildConfigWithDefault (defaultValue :: CompleteAppConfig)[fileCfg, envCfg])
