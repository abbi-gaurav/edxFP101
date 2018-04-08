module Parse where

import           Control.Applicative
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Int
import           Data.Word

data ParseState = ParseState {
  string   :: L.ByteString
  , offset :: Int64
  } deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }


identity :: a -> Parse a
identity a = Parse (\s -> Right(a,s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err          -> Left err
    Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset}

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
  case L.uncons (string initState) of
    Nothing ->
      bail "no more input"
    Just (byte, remainder) ->
      putState newState ==> \_ ->
      identity byte
      where newState = initState { string = remainder,
                                 offset = newOffset
                               }
            newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right(s,s))

putState :: ParseState -> Parse ()
putState s = Parse(\_ -> Right((),s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
                         "byte offset" ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left errMsg -> Left errMsg
            Right (firstResult, newState) -> runParse (secondParser firstResult) newState

instance Functor Parse where
  fmap f parser = parser ==> (\result -> identity (f result))

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = fmap w2c parseByte

peekByte :: Parse (Maybe Word8)
peekByte = let ps = getState
               f  = fmap fst . L.uncons . string
           in fmap f ps

peekChar :: Parse (Maybe Char)
--peekChar = fmap (fmap w2c) peekByte
peekChar = (fmap w2c) <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = ((fmap p) <$> peekByte) ==> (\maybeTrue ->
                                             if maybeTrue == Just True
                                             then parseByte ==> (\byte -> (byte:) <$> (parseWhile p))
                                             else identity []
                                           )
