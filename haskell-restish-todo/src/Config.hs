{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

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

data TaskStoreConfig f = TaskStoreConfig {tscDBFilePath :: f FilePath}

data FancyAppConfig f = FancyAppConfig
                        {
                          facHost         :: f Host
                        , facPort         :: f Port
                        , taskStoreConfig :: f (TaskStoreConfig f)
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
type CompleteTaskStoreConfig = TaskStoreConfig Identity
deriving instance Generic CompleteTaskStoreConfig
deriving instance Eq CompleteTaskStoreConfig
deriving instance Show CompleteTaskStoreConfig
deriving instance FromJSON CompleteTaskStoreConfig

type CompleteAppConfig = FancyAppConfig Identity
deriving instance Generic CompleteAppConfig
deriving instance Eq CompleteAppConfig
deriving instance Show CompleteAppConfig
deriving instance FromJSON CompleteAppConfig

type PartialTaskStoreConfig = TaskStoreConfig Maybe
deriving instance Generic PartialTaskStoreConfig
deriving instance Eq PartialTaskStoreConfig
deriving instance Show PartialTaskStoreConfig
deriving instance FromJSON PartialTaskStoreConfig

type PartialAppConfig = FancyAppConfig Maybe
deriving instance Generic PartialAppConfig
deriving instance Eq PartialAppConfig
deriving instance Show PartialAppConfig
deriving instance FromJSON PartialAppConfig

defaultHost :: Host
defaultHost = "localhost"

defaultPort :: Port
defaultPort = 5000

defaultTaskStoreFilePath :: FilePath
defaultTaskStoreFilePath = ":memory:"

class (FromJSON cfg) => FromJSONFile cfg where
  fromJSONFile :: FP.FilePath -> IO (Either ConfigurationError cfg)

instance FromJSONFile PartialAppConfig where
  fromJSONFile path = decodeAndTransformError <$> DBL.readFile convertedPath
    where
      convertedPath = FCOS.encodeString path

      decodeAndTransformError :: DBL.ByteString -> Either ConfigurationError PartialAppConfig
      decodeAndTransformError = first ConfigParseError . eitherDecode

class (FromJSONFile cfg) => FromTOMLFile cfg where
  fromTOMLFile :: FP.FilePath -> IO (Either ConfigurationError cfg)

instance FromTOMLFile PartialAppConfig where
  fromTOMLFile path = flattenEither . convertAndParse . parseTOML
                      <$> DTI.readFile convertedPath
    where
      convertedPath = FCOS.encodeString path
      parseTOML = first TOMLParseError . parseTomlDoc ""
      convertAndParse = second (parseEither parseJSON . toJSON)
      flattenEither v = case v of
                          Right (Right cfg) -> Right cfg
                          Right (Left err)  -> Left (ConfigParseError err)
                          Left err          -> Left err

class FromENV cfg where
  fromEnv :: ProcessEnvironment -> cfg

instance FromENV PartialTaskStoreConfig where
  fromEnv pEnv = TaskStoreConfig {tscDBFilePath = prop "TASK_STORE_FILE_PATH"}
    where
      env :: [(String,String)]
      env = getProcessEnv pEnv

      prop :: String -> Maybe String
      prop = flip lookup env

instance FromENV PartialAppConfig where
  fromEnv pEnv = FancyAppConfig { facHost = prop "host"
                                , facPort = join $ readMaybe <$> prop "port"
                                , taskStoreConfig = Just $ fromEnv pEnv
                                }
                 where
                   env :: [(String,String)]
                   env = getProcessEnv pEnv

                   prop :: String -> Maybe String
                   prop = flip lookup env

-- | The class of configurations that can absorb partials of themselves to maintain a whole
class AbsorbPartial complete partial where
  absorbPartial :: complete -> partial -> complete

instance AbsorbPartial CompleteTaskStoreConfig PartialTaskStoreConfig where
  absorbPartial :: CompleteTaskStoreConfig -> PartialTaskStoreConfig -> CompleteTaskStoreConfig
  absorbPartial c p = TaskStoreConfig {tscDBFilePath = maybe (tscDBFilePath c) Identity (tscDBFilePath p)}

instance AbsorbPartial CompleteAppConfig PartialAppConfig where
  absorbPartial :: CompleteAppConfig -> PartialAppConfig -> CompleteAppConfig
  absorbPartial c p = FancyAppConfig { facHost = maybe (facHost c) Identity (facHost p)
                                     , facPort = maybe (facPort c) Identity (facPort p)
                                     , taskStoreConfig = Identity $ absorbPartial tsc maybeTsc
                                     }
    where
      tsc = runIdentity $ taskStoreConfig c
      maybeTsc = maybe mempty id $ taskStoreConfig p

instance Semigroup CompleteTaskStoreConfig where
  a <> b = b

instance Semigroup CompleteAppConfig where
  a <> b = b

instance Monoid CompleteTaskStoreConfig where
  mempty = TaskStoreConfig (Identity defaultTaskStoreFilePath)


instance Monoid CompleteAppConfig where
  mempty = FancyAppConfig (Identity defaultHost) (Identity defaultPort) (Identity mempty)

-- | value from b overrides
instance Semigroup PartialTaskStoreConfig where
  a <> b = TaskStoreConfig {tscDBFilePath = resolveMaybes tscDBFilePath}
    where
      resolveMaybes :: (PartialTaskStoreConfig -> Maybe a) -> Maybe a
      resolveMaybes getter = getter b <|> getter a

instance Semigroup PartialAppConfig where
  a <> b = FancyAppConfig { facHost = resolveMaybes facHost
                          , facPort = resolveMaybes facPort
                          , taskStoreConfig = resolveMaybes taskStoreConfig
                          }
           where
             resolveMaybes :: (PartialAppConfig -> Maybe c) -> Maybe c
             resolveMaybes getter = getter b <|> getter a

instance Monoid PartialTaskStoreConfig where
  mempty = TaskStoreConfig Nothing

instance Monoid PartialAppConfig where
  mempty = FancyAppConfig Nothing Nothing Nothing

rightOrThrow :: Exception a => Either a b -> IO b
rightOrThrow e = case e of
                   (Left err) -> throw err
                   (Right v)  -> return v

buildConfigWithDefault :: CompleteAppConfig -> [PartialAppConfig] -> CompleteAppConfig
buildConfigWithDefault orig partials = orig `absorbPartial` combinedPartials
  where
    combinedPartials :: PartialAppConfig
    combinedPartials = Prelude.foldl (<>) (mempty :: PartialAppConfig) partials

makeAppConfig :: Maybe FilePath -> ProcessEnvironment -> IO (Either ConfigurationError CompleteAppConfig)
makeAppConfig maybeStrPath env = try generateConfig
  where
    maybePath :: Maybe FCOS.FilePath
    maybePath = FCOS.fromText . pack <$> maybeStrPath

    extension :: Maybe Text
    extension = join $ FP.extension <$> maybePath

    isJsonExtension :: String -> Bool
    isJsonExtension = (== "json")

    isTOMLExtension = (== "toml")

    isJSONFile = fromMaybe False $ (isJsonExtension . unpack) <$> extension

    isTOMLFile :: Bool
    isTOMLFile = fromMaybe False $ (isTOMLExtension . unpack) <$> extension

    pathExtensionIsInvalid :: Bool
    pathExtensionIsInvalid = not $ isJSONFile || isTOMLFile

    pathInvalidExtensionErr :: ConfigurationError
    pathInvalidExtensionErr = InvalidPath (fromMaybe "<no path>" maybePath) "Path is invalid (must be either .json or .toml patg)"

    envCfg :: PartialAppConfig
    envCfg = fromEnv env :: PartialAppConfig

    getFileConfig :: FCOS.FilePath -> IO (Either ConfigurationError PartialAppConfig)
    getFileConfig = if isJSONFile then fromJSONFile else fromTOMLFile

    defaultCfg :: CompleteAppConfig
    defaultCfg = mempty

    buildFromEnv :: IO CompleteAppConfig
    buildFromEnv = pure $ absorbPartial defaultCfg envCfg

    generateConfig :: IO CompleteAppConfig
    generateConfig = maybe buildFromEnv buildFromPathAndEnv maybePath


    buildFromPathAndEnv path = when pathExtensionIsInvalid (throw pathInvalidExtensionErr)
                               >> getFileConfig path
                               >>= rightOrThrow
                               >>= \fileCfg -> pure (buildConfigWithDefault (mempty :: CompleteAppConfig)[fileCfg, envCfg])
