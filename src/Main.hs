module Main where

import qualified System.Environment
import qualified System.Exit
import Data.Maybe
import Data.Char
import Text.Read
import Data.Bits

import Data.Word
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Internal as BS (c2w, w2c)

main :: IO ()
main = do
  cliArgs <- System.Environment.getArgs
  args <- return $ parseArgs cliArgs
  case args of
    Nothing -> usage
    Just a -> putStrLn (show a)
  putStrLn $ show $ run (satisfy (\x -> elem x ['a', 'b'])) "caazab"

data Args = Args {
  spec :: String,
  dataPath :: FilePath
} deriving (Show)

-- Parse cli arguments
parseArgs :: [String] -> Maybe Args
parseArgs ("-h":_) = Nothing
parseArgs ("--help":_) = Nothing
parseArgs ("help":_) = Nothing
parseArgs [specString,dataPath] = Just $ Args specString dataPath
parseArgs _ = Nothing

usage :: IO ()
usage = putStrLn "Usage: protop [-h] \"spec string\" path/to/data" >> System.Exit.exitWith (System.Exit.ExitFailure 1)

-- Parser
-- https://stackoverflow.com/questions/20660782/writing-a-parser-from-scratch-in-haskell
type Error = String
newtype Parser a = MakeParser { run :: String -> (String, Either Error a)}

instance Functor Parser where
  fmap f (MakeParser pf) = MakeParser $ \s -> case pf s of
    (s2, either) -> (s2, fmap f either)


-- Consume one character, return tail with parsed char or error
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = MakeParser $ \s -> case s of
  []                -> ([], Left "Input exhausted")
  (h:t) | f h       -> (t, Right h)
        | otherwise -> (t, Left $ "Failed to match")

-- Given string s construct parser that matches exactly s
parseString :: String -> Parser String
parseString matchS = MakeParser $ \s -> (innerParser s matchS)
  where
  innerParser s [] = (s, Right "")
  innerParser s (matchH:matchT) = case run (satisfy (== matchH)) s of
    (s2, Left err) -> (s2, Left err)
    (s2, Right a) -> case innerParser s2 matchT of
      (s3, Left err) -> (s3, Left err)
      (s3, Right as) -> (s3, Right (a : as))

-- Attempt parser, but revert to original if failed
tryParser :: Parser a -> Parser a
tryParser (MakeParser f) = MakeParser $ \s -> case f s of
  (_, Left err) -> (s, Left err)
  x -> x

-- Try first parser, if it fails run second parser
orParser :: Parser a -> Parser a -> Parser a
orParser (MakeParser f1) (MakeParser f2) = MakeParser $ \s -> case f1 s of
  (_, Left err) -> f2 s
  x -> x

-- Try parsers (in order), return first that succeeds
-- Like or parser, but takes a list of parsers
anyParser :: [Parser a] -> Parser a
anyParser [] = MakeParser (\x -> (x, Left "Failed to parse, anyParser with no parsers"))
anyParser (p:ps) = foldr (orParser) p ps

-- Match 0 or more
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (MakeParser f) = MakeParser newP
  where
    newP s = case (f s) of
      (_, Left err) -> (s, Right [])
      (s2, Right a) -> case newP s2 of
        (s3, Left err) -> (s3, Left err)
        (s3, Right as) -> (s3, Right (a : as))

oneOrMore :: Parser a -> Parser [a]
oneOrMore (MakeParser f) = MakeParser $ \s -> case f s of
  (s2, Left err) -> (s2, Left err)
  (s2, Right a) ->
    let zeroOrMoreParser = zeroOrMore (MakeParser f)
    in case (run zeroOrMoreParser) s2 of
      (s3, Left err) -> (s3, Left err)
      (s3, Right as) -> (s3, Right (a:as))

-- Apply first parser until error, then apply second parser, combine results
-- If either fails return failure
andThen :: Parser a -> Parser b -> Parser (a,b)
andThen (MakeParser fa) (MakeParser fb) = MakeParser joined
  where
    joined s = case fa s of
      (s2, Left err) -> (s2, Left err)
      (s2, Right a) -> case fb s2 of
        (s3, Left err) -> (s3, Left err)
        (s3, Right b) -> (s3, Right (a,b))

takeFirst :: Parser (a,b) -> Parser a
takeFirst p = fmap fst p

takeSecond :: Parser (a,b) -> Parser b
takeSecond p = fmap snd p

castParser :: (Show a) => (a -> Maybe b) -> Parser a -> Parser b
castParser castF (MakeParser f) = MakeParser $ \s -> case f s of
  (s2, Left err) -> (s2, Left err)
  (s2, Right as) -> case (castF as) of
      Just x -> (s2, Right x)
      Nothing -> (s2, Left ("Failed to cast '" ++  (show as)))

castInt :: Parser String -> Parser Integer
castInt = castParser (\as -> readMaybe as :: Maybe Integer)

matchDigit = satisfy (`elem` ['0','1'..'9'])
matchSpace = satisfy (== ' ')

matchUnitBits = parseString "bits"
matchUnitBytes = parseString "bytes"
matchUnit = orParser matchUnitBits matchUnitBytes

parseInt :: Parser Integer
parseInt = (castInt (oneOrMore matchDigit))

-- Reads spec input "4 4 8 8" into [4, 4, 8, 8]
parseItems :: Parser [Integer]
parseItems = oneOrMore (takeFirst (parseInt `andThen` matchSpace))

-- Reads spec: "4 4 8 8 bits" into ([4, 4, 8, 8], "bits")
parseSpec :: Parser ([Integer], String)
parseSpec = parseItems `andThen` matchUnit

matchHex = satisfy (`elem` "01234567890ABCDEFabcdef")

-- Parse hex will read hex values (casting to int) and ignore spaces
parseHex :: Parser [Integer]
parseHex =
  let parseHexAndIgnoreSpace = takeFirst (andThen matchHex (zeroOrMore matchSpace))
  in
    zeroOrMore $ castHex parseHexAndIgnoreSpace
castHex = castParser (\a -> readMaybeHex (Data.Char.toUpper a))

-- fromIntegral casts from Int -> Word8, then BS.pack makes into a bytestring
parseHexToByteString :: Parser BS.ByteString
parseHexToByteString = BS.pack <$> hexDigitsToWord8 <$> parseHex

-- After parsing hex into digits, we'd like to convert it into a bytestring (byte array)
-- Each hex digit corresponds to 4 bits, so we want to read 2 digits at a time for each byte
-- Note, in the case of odd digits, the highest hex digit should be treated as 0* not the lowest.
-- For a simple workaround, reverse, recursively parse two hex digits at a type, then reverse aagain
hexDigitsToWord8 :: [Integer] -> [Data.Word.Word8]
hexDigitsToWord8 = reverse . f . reverse
  where
    f :: [Integer] -> [Data.Word.Word8]
    f [] = []
    f [x] = [fromIntegral x]
    f (x1:x2:xs) = [fromIntegral (x1 + x2 * 16)] ++ f xs

readMaybeHex :: Char -> Maybe Integer
readMaybeHex '0' = Just 0
readMaybeHex '1' = Just 1
readMaybeHex '2' = Just 2
readMaybeHex '3' = Just 3
readMaybeHex '4' = Just 4
readMaybeHex '5' = Just 5
readMaybeHex '6' = Just 6
readMaybeHex '7' = Just 7
readMaybeHex '8' = Just 8
readMaybeHex '9' = Just 9
readMaybeHex 'A' = Just 10
readMaybeHex 'B' = Just 11
readMaybeHex 'C' = Just 12
readMaybeHex 'D' = Just 13
readMaybeHex 'E' = Just 14
readMaybeHex 'F' = Just 15
readMaybeHex x = Nothing

---------------------------------------------------------------------
-- Bytestring

showBytestringHex :: BS.ByteString -> String
showBytestringHex bs =
  let bshex = (map BS.w2c) . BS.unpack . BSB.toLazyByteString . BSB.lazyByteStringHex
  in "0x" ++ (bshex bs)

slice :: Integer -> Integer -> BS.ByteString -> BS.ByteString
slice start end = (BS.drop $ fromIntegral start) . (BS.take (fromIntegral $ end))

sliceBits :: Integer -> Integer -> BS.ByteString -> BS.ByteString
sliceBits start end =
  let
    (startByte, endByte) = bitIndexToByteIndex (start,end)
  in
    slice startByte endByte

sliceBits2 :: Integer -> Integer -> BS.ByteString -> BS.ByteString
sliceBits2 start end bytes =
  let
    bits = byteStringToBitString bytes
    sliced = drop (fromIntegral start) $ take (fromIntegral end) bits
  in
    bitStringToByteString sliced

bitIndexToByteIndex :: (Integer, Integer) -> (Integer, Integer)
bitIndexToByteIndex (start,end) =
  let
    startByte = toInteger $ floor $ (fromInteger start :: Float) / 8
    endByte = toInteger $ ceiling $ (fromInteger end :: Float) / 8
  in
    (startByte, endByte)


byteStringToBitString :: BS.ByteString -> [Bool]
byteStringToBitString bs = concatMap word8ToBits (BS.unpack bs)

bitStringToByteString :: [Bool] -> BS.ByteString
bitStringToByteString = BS.pack . toWords

toWords :: [Bool] -> [Word8]
toWords [] = []
toWords bs =
  [bitsToWord8 (take 8 bs)] ++ (toWords (drop 8 bs))

-- word8ToBits and bitsToWord8 reverse to keep Big Endian
word8ToBits :: Data.Word.Word8 -> [Bool]
word8ToBits w =
  let
    wordWithIndex = zip [0..] $ replicate 8 w
    nthBit = \x -> testBit (snd x) (fst x)
  in
    reverse $ fmap nthBit wordWithIndex

bitsToWord8 :: [Bool] -> Data.Word.Word8
bitsToWord8 bs =
  let
    filledWithIndex = zip [0..] $ reverse $ replicate (8 - length bs) False ++ bs
    inner = \(i,b) acc -> if b then setBit acc i else acc
  in
    foldr inner (fromIntegral 0 :: Word8) filledWithIndex
