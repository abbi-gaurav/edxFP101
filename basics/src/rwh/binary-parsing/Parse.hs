module Parse where

import           Common
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
assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert _ err  = bail err

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& q = p ==> (\_ -> q)

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith func predicate =
  let word8ListParse     = parseWhile (predicate . func) :: Parse [Word8]
      func4Word8List     = (fmap func)
  in func4Word8List <$> word8ListParse

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
                                            if null digits
                                            then bail "no more inputs"
                                            else
                                              let n = read digits
                                              in if n < 0
                                                 then bail "integer overflow"
                                                 else identity n

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
                 let n' = fromIntegral n
                     (h, t) = L.splitAt n' (string st)
                     st' = st {offset = offset st + L.length h, string = t}
                 in putState st' ==>&
                    assert (L.length h == n') "end of input" ==>&
                    identity h

parseRawPGM =
  let parseChars = parseWhileWith w2c notWhite :: Parse [Char]
  in parseChars ==> \header -> skipSpaces ==>&
  assert (header == "P5") "invalid raw header" ==>&
  parseNat ==> \width -> skipSpaces ==>&
  parseNat ==> \height -> skipSpaces ==>&
  parseNat ==> \maxGrey -> parseByte ==>&
  parseBytes (width * height) ==> \bitMap -> identity (GreyMap width height maxGrey bitMap)
  where notWhite = (`notElem` " \r\n\t")
