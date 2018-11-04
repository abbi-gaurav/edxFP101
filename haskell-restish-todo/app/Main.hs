{-# LANGUAGE RecordWildCards #-}

module Main where

import           Config
import           Control.Monad       (join)
import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative
import           System.Environment  (getEnvironment)
import           Text.Pretty.Simple  (pPrint)

data Options = Options {
  cfgPath :: Maybe FilePath,
  cmd     :: Command
  }
data Command = Serve
             | ShowConfig deriving Eq

parseCommands :: Parser Command
parseCommands = subparser commands
  where
    serverCmd :: ParserInfo Command
    serverCmd = info (pure Serve) (progDesc "Start the server")

    showConfigCmd :: ParserInfo Command
    showConfigCmd = info (pure ShowConfig) (progDesc "Show the configuration")

    commands :: Mod CommandFields Command
    commands = command "server" serverCmd
               <> command "show-config" showConfigCmd

parseOptions :: Parser (Maybe FilePath)
parseOptions = optional
               $ strOption (long "config"
                           <> short 'c'
                           <> metavar "FILENAME"
                           <> help "Configuration file (.json/.toml)")

parseCmdLine :: Parser Options
parseCmdLine = Options <$> parseOptions <*> parseCommands

pullEnvironment :: IO ProcessEnvironment
pullEnvironment = ProcessEnvironment <$> getEnvironment

runServer :: Options -> IO ()
runServer Options{cfgPath=path} = pullEnvironment
                                   >>= makeAppConfig path
                                   >> server

showConfig :: Options -> IO ()
showConfig Options{cfgPath=path} = pullEnvironment
                                   >>= makeAppConfig path
                                   >>= pPrint

-- | Start up the server and serve requests
server :: IO ()
server = putStrLn "<SERVER START>"


main :: IO ()
main = parseOptions
       >>= process
  where
    cmdParser :: ParserInfo Options
    cmdParser = info parseCmdLine idm

    parseOptions :: IO Options
    parseOptions = execParser cmdParser

    process :: Options -> IO ()
    process opts = case cmd opts of
                     Serve      -> runServer opts
                     ShowConfig -> showConfig opts
