module Main where

import qualified System.Environment
import qualified System.Exit
import Data.Maybe

main :: IO ()
main = do
  cliArgs <- System.Environment.getArgs
  args <- return $ parseArgs cliArgs
  case args of
    Nothing -> usage
    Just a -> putStrLn (show a)

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
