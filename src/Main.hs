module Main where

import qualified System.Environment
import qualified System.Exit
import Data.Maybe
import Text.Read

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
takeFirst (MakeParser f) = MakeParser $ \s -> case f s of
  (s2, Left err) -> (s2, Left err)
  (s2, Right ab) -> (s2, Right (fst ab))

castInt :: Parser String -> Parser Integer
castInt (MakeParser f) = MakeParser $ \s -> case f s of
  (s2, Left err) -> (s2, Left err)
  (s2, Right as) ->
    let
      maybeInt = readMaybe as :: Maybe Integer
    in case maybeInt of
      Just i -> (s2, Right i)
      Nothing -> (s2, Left ("Failed to cast '" ++ as ++  "' to int"))

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
