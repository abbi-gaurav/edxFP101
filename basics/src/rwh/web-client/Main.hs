{- | To run
stack exec -- pods-exe add "https://player.fm/series/the-haskell-cast"
-}
module Main where

import           Database.HDBC
import           Network.Socket     (withSocketsDo)
import           PodDB
import           PodDownload
import           PodTypes
import           System.Environment

main = withSocketsDo $ handleSqlError $
  do args <- getArgs
     dbh <- connect "pod.db"
     case args of
       ["add", url] -> add dbh url
       ["update"]   -> update dbh
       ["download"] -> download dbh
       ["fetch"]    -> do update dbh
                          download dbh
       _ -> syntaxError
     disconnect dbh

add :: IConnection conn => conn -> String -> IO ()
add dbh url =
  do addPodcast dbh pc
     commit dbh
  where pc = Podcast {castId = 0, castURL = url}

update :: IConnection conn => conn -> IO ()
update dbh =
  do pcList <- getPodcasts dbh
     mapM_ procPodcast pcList
  where procPodcast pc =
          do putStrLn $ "Updating podcast from " ++ (castURL pc)
             updatePodcastFromFeed dbh pc

download :: IConnection conn => conn -> IO ()
download dbh =
  do pcList <- getPodcasts dbh
     mapM_ procPodcast pcList
  where procPodcast pc =
          do putStrLn $ "Considering " ++ (castURL pc)
             episodeList <- getPodcastEpisodes dbh pc
             let dleps = filter (\ep -> epDone ep == False)
                         episodeList
             mapM_ procEpisode dleps
        procEpisode ep = do
          putStrLn $ "Downloading " ++ (epURL ep)
          getEpisode dbh ep

syntaxError :: IO ()
syntaxError = putStrLn
  "Usage: pod command [args]\n\
  \\n\
  \pod add url      Adds a new podcast with the given URL\n\
  \pod download     Downloads all pending episodes\n\
  \pod fetch        Updates, then downloads\n\
  \pod update       Downloads podcast feeds, looks for new episodes\n"
