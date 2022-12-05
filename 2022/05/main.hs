#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.List (transpose, foldl')

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
  ["move", count, "from", from, "to", to] ->
    -- We subtract one because we want 0-based indexing.
    Move (read count) ((read from) - 1) ((read to) - 1)
  z -> error $ "invalid input: " ++ (unwords z)

popAt :: Int -> [[Char]] -> ([[Char]], Char)
popAt i stacks =
  let
    stack = stacks !! i
    (result, stack') = (head stack, tail stack)
    before = take i stacks
    after = drop (i + 1) stacks
  in
    (before ++ [stack'] ++ after, result)

pushAt :: Int -> Char -> [[Char]] -> [[Char]]
pushAt i x stacks =
  let
    stack = stacks !! i
    before = take i stacks
    after = drop (i + 1) stacks
  in
    before ++ [x : stack] ++ after

executeOnce :: Int -> Int -> [[Char]] -> [[Char]]
executeOnce from to stacks =
  let
    (stacks', x) = popAt from stacks
  in
    pushAt to x stacks'

execute :: Move -> [[Char]] -> [[Char]]
execute move stacks = case move of
  Move 0 _ _ -> stacks
  Move n from to -> execute (Move (n - 1) from to) (executeOnce from to stacks)

main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let
    (stacks, movesStrings) = parseStacks $ lines fileContents
    moves = fmap parseMove movesStrings
    finalState = foldl' (flip execute) stacks moves
  putStrLn $ fmap head finalState
   
