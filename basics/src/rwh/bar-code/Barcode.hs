module Barcode where

import           Data.Array                 (Array (..), bounds, elems, indices,
                                             ixmap, listArray, (!))

import           Control.Applicative        ((<$>))
import           Control.Monad              (forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char                  (digitToInt)
import           Data.Function
import           Data.Ix                    (Ix (..))
import           Data.List                  (foldl', group, sort, sortBy, tails)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Ratio                 (Ratio)
import           Data.Word                  (Word8)
import           Parse
import           System.Environment         (getArgs)

-- http://www.barcodeisland.com/ean13.phtml
--101
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - ((sum products) `mod` 10)
  where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f ds = zipWith ($) (cycle [f,id]) ds

leftOddList :: [String]
leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList :: [String]
rightList = fmap (\str -> fmap complement str) leftOddList
  where complement '0' = '1'
        complement '1' = '0'

leftEvenList :: [String]
leftEvenList = map reverse rightList

parityList :: [String]
parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs
  where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

-- strict left fold
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f acc array = go acc (indices array)
  where go acc (i : is) = let acc' = f acc (array ! i)
                          in acc' `seq` go acc' is
        go acc _        = acc

-- TODO figure out more efficient way to get startindex
-- strict fold with accumulator as first element
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f array = let startIndex = indices array !! 1
                     endIndex = snd (bounds array)
                 in foldA f (array ! fst (bounds array)) (ixmap (startIndex , endIndex) id array)

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first : rest) =
  outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 6 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' x = (leftOddCodes ! x)
leftEncode '0' x = (leftEvenCodes ! x)

rightEncode :: Int -> String
rightEncode x = (rightCodes ! x)

outerGuard = "101"
centerGuard = "01010"

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

type PixMap = Array (Int, Int) RGB

parseRawPPM :: Parse PixMap
parseRawPPM =
  parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
  assert (header == "P6") "invalid raw header" ==>&
  parseNat ==> \width -> skipSpaces ==>&
  parseNat ==> \height -> skipSpaces ==>&
  parseNat ==> \maxValue ->
  assert(maxValue == 255) "max value out of spec" ==>&
  parseByte ==>&
  parseTimes (width * height) parseRGB ==> \pxs ->
  identity(listArray ((0,0), (width - 1, height - 1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r,g,b)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> ((x:) <$>  (parseTimes (n-1) p))

luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

type GreyMap = Array (Int, Int) Pixel

pixMapToGreyMap :: PixMap -> GreyMap
pixMapToGreyMap pm = luminance <$> pm

data Bit = Zero | One
  deriving (Eq, Show)


threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n arr = binary <$> arr
  where binary i | i < pivot = Zero
                 | otherwise = One
        pivot    = round (least + (greatest - least) * n)
        least    = fromIntegral (choose (<) arr)
        greatest = fromIntegral (choose (>) arr)
        choose f = foldA1 (\x y -> if f x y then x else y)

type Run = Int
type RunLength a = [(Run,a)]

runLength :: Eq a => [a] -> RunLength a
runLength  = (map rle) . group
  where rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = (map fst) . runLength


type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
  where divide d = fromIntegral d / divisor
        divisor = fromIntegral (sum xs)

type ScoreTable = [[Score]]

--- SRL ~ scaled run length
asSRL :: [String] -> ScoreTable
asSRL strings = map (scaleToOne . runLengths) strings

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

distance :: [Score] -> [Score] -> Score
--- (sum . map abs) (zipWith (-) a b)
distance a b = sum . map abs $ zipWith (-) a b

type Digit = Word8

bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores st rs = take 3 . sort $ scores
  where scores = zip [distance d (scaleToOne rs) | d <- st] digits
        digits = [0..9]

data Parity a = Even a | Odd a | None a
  deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a)  = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a)  = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
  fmap = parityMap

compareWithoutParity :: Ord a => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
              (
                (map Odd (bestScores leftOddSRL ps))
                ++
                (map Even (bestScores leftEvenSRL ps))
              )
bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = (map None) . (bestScores rightSRL)

chunkWith :: ([a] -> ([a],[a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h,t) = f xs
                     in h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_,One):_) = []
candidateDigits rle         | length rle < 59 = []
candidateDigits rle
  | any null match = []
  | otherwise = map (map (fmap snd)) match
  where match      = map bestLeft left ++ map bestRight right
        left       = chunksOf 4 . take 24 . drop 3 $ runLengths
        right      = chunksOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle

type SeqMap a = M.Map Digit [a]
type DigitMap = SeqMap Digit
type ParityMap = SeqMap (Parity Digit)

updateMap :: Parity Digit     -- ^ new digit
          -> Digit            -- ^ existing key
          -> [Parity Digit]   -- ^ existing digit sequence
          -> ParityMap        -- ^ map to update
          -> ParityMap
updateMap parityDigit key seq map = insertMap key (fromParity parityDigit) (parityDigit:seq) map

insertMap :: Digit -> Digit -> [a] -> SeqMap a -> SeqMap a
insertMap key digit val m = val `seq` M.insert key' val m
  where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new parityDigit = new `M.union` M.foldrWithKey (updateMap parityDigit) M.empty old

incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = (foldl' incorporateDigits (M.singleton 0 [])) . (mapEveryOther (map (fmap (* 3))))

firstDigit :: [Parity a] -> Digit
firstDigit = snd
             . head
             . bestScores paritySRL
             . runLengths
             . map parityBit
             . take 6
  where parityBit (Even _) = Zero
        parityBit (Odd _)  = One

addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldrWithKey updateFirst M.empty

updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit : renormalize qes)
  where renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq

buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -)
           . addFirstDigit
           . finalDigits

solve :: [[Parity Digit]] -> [[Digit]]
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
  where checkDigits       = map fromParity (last xs)
        m                 = buildMap (init xs)
        addCheckDigit m k = (++[k]) <$> M.lookup k m
