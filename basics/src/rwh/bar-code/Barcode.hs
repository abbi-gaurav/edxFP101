module Barcode where

import           Data.Array                 (Array (..), bounds, elems, indices,
                                             ixmap, listArray, (!))

import           Control.Applicative        ((<$>))
import           Control.Monad              (forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char                  (digitToInt)
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
  where (left, right) = splitAt 5 rest
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
