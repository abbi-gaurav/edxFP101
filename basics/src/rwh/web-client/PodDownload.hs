module PodDownload where

import           Data.Maybe
import           Database.HDBC
import           Network.HTTP
import           Network.URI
import           PodDB
import           PodParser
import           PodTypes
import           System.IO

downloadURL :: String -> IO (Either String String)
downloadURL url = do
  resp <- simpleHTTP request
  case resp of
    Left x -> return $ Left ("Error connecting " ++ show x)
    Right r -> case rspCode r of
      (2,_,_) -> return $ Right (rspBody r)
      (3,_,_) -> case findHeader HdrLocation r of
        Nothing  -> return $ Left (show r)
        Just url -> downloadURL url
      _ -> return $ Left (show r)
    where request = Request {
            rqURI = uri,
            rqMethod = GET,
            rqHeaders = [],
            rqBody = ""
            }
          uri = fromJust $ parseURI url

updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbh podcast =
  do resp <- downloadURL (castURL podcast)
     case resp of
       Left x    -> putStrLn x
       Right doc -> updateDB doc
  where updateDB doc =
          do mapM_ (addEpisode dbh) episodes
             commit dbh
          where feed = parse doc (castURL podcast)
                episodes = map (item2ep podcast)(items feed)


getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbh ep = do
  resp <- downloadURL (epURL ep)
  case resp of
    Left x -> do putStrLn x
                 return Nothing
    Right doc -> do
      file <- openBinaryFile fileName WriteMode
      hPutStr file doc
      hClose file
      updateEpisode dbh (ep {epDone = True})
      commit dbh
      return (Just fileName)
  where fileName = "pod." ++ (show . castId . epCast $ ep) ++ "." ++ (show (epId ep)) ++ ".mp3"
