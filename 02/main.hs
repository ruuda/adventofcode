#!/usr/bin/env ghci

import Control.Monad (forM_)
import Data.Char (isDigit)

data Entry = Entry
  { entryMin :: Int
  , entryMax :: Int
  , entryChar :: Char
  , entryPass :: String
  } deriving (Show)

parseEntry :: String -> Entry
parseEntry line =
  let
    (minStr, '-' : more0) = span isDigit line
    (maxStr, ' ' : char : ':' : ' ' : passwd) = span isDigit more0
  in
    Entry (read minStr) (read maxStr) char passwd

isValid :: Entry -> Bool
isValid entry =
  let
    n = length $ filter (== entryChar entry) $ entryPass entry
  in
    n >= entryMin entry && n <= entryMax entry

main :: IO ()
main = do
  entries <- (fmap parseEntry . lines) <$> readFile "input.txt"
  putStrLn $ show $ length $ filter isValid entries
