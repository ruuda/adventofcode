#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.List (transpose)

parseRow :: String -> [Char]
parseRow input = case input of
  _ : ch : _ : ' ' : more -> ch : parseRow more
  _ : ch : _ : "" -> [ch]
  z -> error $ "invalid input: " ++ z

-- Parse the inputs into a list of stacks, and return the remaining unprocessed
-- lines.
parseStacks :: [String] -> ([[Char]], [String])
parseStacks = go []
  where
    isEndRow = all (\ch -> isDigit ch || ch == ' ')
    finalize stacks = fmap (filter (/= ' ')) $ transpose stacks
    go stacks (line : more) =
      let
        row = parseRow line
      in
        if isEndRow row
          then (finalize stacks, tail more)
          else go (stacks ++ [row]) more

data Move = Move
  { mCount :: Int
  , mFrom :: Int
  , mTo :: Int
  } deriving (Show)

parseMove :: String -> Move
parseMove line = case words line of
  ["move", count, "from", from, "to", to] -> Move (read count) (read from) (read to)
  z -> error $ "invalid input: " ++ (unwords z)

main :: IO ()
main = do
  fileContents <- readFile "example.txt"
  let
    (stacks, movesStrings) = parseStacks $ lines fileContents
    moves = fmap parseMove movesStrings
  putStrLn $ show stacks
  forM_ moves print
