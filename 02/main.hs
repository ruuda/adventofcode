#!/usr/bin/env runhaskell

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
    (minStr, '-' : more) = span isDigit line
    (maxStr, ' ' : char : ':' : ' ' : passwd) = span isDigit more
  in
    Entry (read minStr) (read maxStr) char passwd

isValid1 :: Entry -> Bool
isValid1 entry =
  let
    n = length $ filter (== entryChar entry) $ entryPass entry
  in
    n >= entryMin entry && n <= entryMax entry

isValid2 :: Entry -> Bool
isValid2 (Entry i j c pass) =
  let
    isC k = c == (pass) !! (k - 1)
  in
    isC i /= isC j

main :: IO ()
main = do
  entries <- (fmap parseEntry . lines) <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> (show $ length $ filter isValid1 entries)
  putStrLn $ "Part 2: " <> (show $ length $ filter isValid2 entries)
