{-# LANGUAGE RecordWildCards #-}

module Main where

import           Config
import           Control.Monad       (join)
import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative (CommandFields, Mod, Parser, ParserInfo,
                                      argument, command, execParser, idm, info,
                                      str, subparser)

newtype Options = Options {cms :: Command}
data Command = Serve Host Port

-- | Start up the server and serve requests
server :: IO ()
server = putStrLn "<SERVER START>"

-- | CLI options parser
opts :: Parser (IO ())
opts = subparser commands
  where
    -- | IO action that produces an IO action.
    --   This was necessary due to using optparse-applicative like I am, by returning an IO action straight after `info` in serverCmd
    serverAction :: Parser (IO ())
    serverAction = pure server

    serverCmd :: ParserInfo (IO ())
    serverCmd = info (serverAction) idm

    commands :: Mod CommandFields (IO ())
    commands = command "server" serverCmd

main :: IO ()
main = join $ execParser parser
  where
    parser::ParserInfo (IO ())
    parser = info opts idm
