#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Char (isDigit)
import Prelude hiding (Left, Right)

data Field = Field
  { fWidth :: Int
  , fHeight :: Int
  , fData :: [[Char]]
  } deriving (Show)

data Heading
  = Right
  | Down
  | Left
  | Up
  deriving (Show)

data Move
  = TurnLeft
  | TurnRight
  | Ahead Int
  deriving (Show)

parseMoves :: String -> [Move]
parseMoves str = case str of
  "" -> []
  _ | isDigit (head str) ->
    let
      (stepsStr, rest) = span isDigit str
    in
      (Ahead $ read stepsStr) : (parseMoves rest)
  ch : rest -> case ch of
    'L' -> TurnLeft : parseMoves rest
    'R' -> TurnRight : parseMoves rest
    _   -> error "Invalid direction."

parseInput :: String -> (Field, [Move])
parseInput contents =
  let
    fileLines = lines contents
    height = (length fileLines) - 2
    fieldData = take height fileLines
    field = Field (length $ head fieldData) height fieldData
    moves = parseMoves $ last fileLines
  in
    (field, moves)

main :: IO ()
main = do
  fileContents <- readFile "example.txt"
  let (field, moves) = parseInput fileContents
  putStrLn $ show field
  putStrLn $ show moves
