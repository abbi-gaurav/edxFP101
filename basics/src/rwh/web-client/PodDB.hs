module PodDB where

import           Control.Monad         (when)
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           PodTypes

connect :: FilePath -> IO Connection
connect fp = do
  dbh <- connectSqlite3 fp
  prepareDB dbh
  return dbh

prepareDB :: IConnection conn => conn -> IO ()
prepareDB dbh = do
  tables <- getTables dbh
  when (not ("podcasts" `elem` tables)) $
    do run dbh "CREATE TABLE podcasts (\
               \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
               \casturl TEXT NOT NULL UNIQUE)" []
       return ()
  when (not ("episodes" `elem` tables)) $
    do run dbh "CREATE TABLE episodes (\
               \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
               \epcastid INTEGER NOT NULL,\
               \epurl TEXT NOT NULL,\
               \epdone INTEGER NOT NULL,\
               \UNIQUE(epcastid, epurl),\
               \UNIQUE(epcastid, epid))" []
       return ()
  commit dbh

addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast =
  handleSql errorHandler $
  do
    run dbh "INSERT INTO podcasts (casturl) VALUES (?)" [toSql (castURL podcast)]
    r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE casturl = ?" [toSql (castURL podcast)]
    case r of
      [[x]] -> return $ podcast {castId = fromSql x}
      y     -> fail $ "addpodcast: unexpectd result: "++ show y
  where errorHandler e =
          do fail $ "Error adding podcast. Does this URL already exists\n" ++ show e

addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh episode =
  run dbh "INSERT OR IGNORE INTO episodes (epcastid, epurl, epdone) \
          \VALUES (?, ?, ?)" [toSql (castId . epCast $ episode), toSql (epURL episode), toSql (epDone episode)]
  >> return ()

updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast =
  run dbh "UPDATE podcasts SET casturl = ? WHERE castid = ?" [toSql (castURL podcast), toSql (castId podcast)]
  >> return ()

updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh ep =
  run dbh "UPDATE episodes SET epcastid = ?, epurl = ?, epdone = ? \
          \WHERE epid = ?" [toSql (castId . epCast $ ep),
                            toSql (epURL ep),
                            toSql (epDone ep)]
  >> return ()

getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts dbh = do
  res <- quickQuery' dbh
         "SELECT castid, casturl FROM podcasts ORDER BY castid" []
  return (map convPodcastRow res)

getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast dbh id = do
  res <- quickQuery' dbh "SELECT castid, casturl FROM podcasts where castid = ?" [toSql id]
  case res of
    [x] -> return (Just (convPodcastRow x))
    []  -> return Nothing
    x   -> fail $ "More than one podcast with same id " ++ show x

getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO [Episode]
getPodcastEpisodes dbh podcast = do
  r <- quickQuery' dbh "SELECT epid, epurl, epdone from episodes where epcastid = ?" [toSql . castId $ podcast]
  return (map convEpisodeRow r)
    where convEpisodeRow [svID, svURL, svDone] = Episode {epId = fromSql svID, epURL = fromSql svURL, epCast = podcast, epDone = fromSql svDone}

removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbh podcast = do
  run dbh "DELETE from episodes where epcastid = ?" [toSql (castId podcast)]
  run dbh "DELETE from podcasts where castid = ?" [toSql (castId podcast)]
  return ()

convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svID, svURL] =
  Podcast {castId = fromSql svID,
            castURL = fromSql svURL}
convPodcastRow x = error $ "can't convert podcast row " ++ show x
