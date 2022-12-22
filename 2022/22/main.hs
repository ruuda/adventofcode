#!/usr/bin/env runhaskell

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.List (foldl')
import Data.Char (isDigit)
import Prelude hiding (Left, Right)

data Board = Board
  { boardWidth :: !Int
  , boardHeight :: !Int
  , boardCells :: [[Char]]
  } deriving (Show)

data Heading
  = Right
  | Down
  | Left
  | Up
  deriving (Show)

turnLeft :: Heading -> Heading
turnLeft = \case
  Right -> Up
  Down  -> Right
  Left  -> Down
  Up    -> Left

turnRight :: Heading -> Heading
turnRight = turnLeft . turnLeft . turnLeft

data Move
  = TurnLeft
  | TurnRight
  | Ahead Int
  deriving (Show)

-- Position on the board.
data Pos = Pos !Heading !Int !Int deriving (Show)

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

parseInput :: String -> (Board, [Move])
parseInput contents =
  let
    fileLines = lines contents
    height = (length fileLines) - 2
    cells = take height fileLines
    board = Board (length $ head cells) height cells
    moves = parseMoves $ last fileLines
  in
    (board, moves)

initialPosition :: Board -> Pos
initialPosition (Board w h cells) =
  let
    -- Find the leftmost open cell.
    x = fst $ head $ filter (\(_i, ch) -> ch == '.') $ zip [0..] $ head cells
    y = 0
  in
    Pos Right x y

boardAt :: Board -> Pos -> Char
boardAt (Board _w _h cells) (Pos _heading x y) =
  let
    row = cells !! y
  in
    -- Some rows are not padded with spaces at the end, fill those in.
    if x < length row then row !! x else ' '

-- Move one step forward while wrapping around the board if needed. The new cell
-- can contain a wall.
advanceTorus :: Board -> Pos -> Pos
advanceTorus board@(Board w h cells) (Pos heading cx cy) = go newX newY
  where
    (newX, newY) = case heading of
      Right -> (cx + 1, cy)
      Down  -> (cx, cy + 1)
      Left  -> (cx - 1, cy)
      Up    -> (cx, cy - 1)

    go x y | x >= w = go (x - w) y
           | x <  0 = go (x + w) y
           | y >= h = go x (y - h)
           | y <  0 = go x (y + h)
             -- If we find an cell that is part of the board, we are done.
           | boardAt board (Pos heading x y) /= ' ' = Pos heading x y
             -- If we find a cell that is not part of the board, step one
             -- further.
           | otherwise = advanceTorus board (Pos heading x y)

-- Maps every face id (as in the illustration in the puzzle) to its four
-- neighboring faces (right, up, left, down), and the number of left turns
-- we need to make when we traverse the edge.
neighbors :: Int -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
neighbors fid = case fid of
  1 -> ((6, 2), (2, 2), (3, 1), (4, 0))
  2 -> ((3, 0), (1, 2), (6, 3), (5, 2))
  3 -> ((4, 0), (1, 3), (2, 0), (5, 1))
  4 -> ((6, 3), (1, 0), (3, 0), (5, 0))
  5 -> ((6, 0), (4, 0), (3, 3), (2, 2))
  6 -> ((1, 2), (4, 1), (5, 0), (2, 1))
  _ -> error "Invalid face id."

faceId :: Int -> Int -> Int
faceId fx fy = case (fx, fy) of
  (2, 0) -> 1
  (0, 1) -> 2
  (1, 1) -> 3
  (2, 1) -> 4
  (2, 2) -> 5
  (3, 2) -> 6
  _ -> error $ "Invalid face coordinate " <> (show (fx, fy)) <> ", should be on the map."

-- Inverse of faceId
facePos :: Int -> (Int, Int)
facePos = \case
  1 -> (2, 0)
  2 -> (0, 1)
  3 -> (1, 1)
  4 -> (2, 1)
  5 -> (2, 2)
  6 -> (3, 2)
  _ -> error "Invalid face id."

-- Move one step forward while wrapping around the cube if needed. This is a
-- huge mess. Probably there exists an elegant coordinate system that makes this
-- less messy, but I have not discovered it yet.
advanceCube :: Board -> Pos -> Pos
advanceCube board@(Board w h cells) (Pos heading cx cy) =
  let
    faceSize = h `div` 3
    (fx, fy) = (cx `div` faceSize, cy `div` faceSize)
    (px, py) = (cx `mod` faceSize, cy `mod` faceSize)
    face = faceId fx fy
    (right, up, left, down) = neighbors face
    rotateLeft (h, x, y) = (turnLeft h, y, faceSize - 1 - x)
    rotateLeftN 0 (h, x, y) = (h, x, y)
    rotateLeftN n (h, x, y) = rotateLeftN (n - 1) (rotateLeft (h, x, y))
    (x, y) = case heading of
      Right -> (px + 1, py)
      Down  -> (px, py + 1)
      Left  -> (px - 1, py)
      Up    -> (px, py - 1)
    moveTo (fid, nLeft) coord = (fid, rotateLeftN nLeft coord)
    (newFace, (heading', px', py')) = case () of
      _ | x >= faceSize -> moveTo right (heading, x - faceSize, y)
        | x <  0        -> moveTo left  (heading, x + faceSize, y)
        | y >= faceSize -> moveTo down  (heading, x, y - faceSize)
        | y <  0        -> moveTo up    (heading, x, y + faceSize)
        | otherwise     -> (face, (heading, x, y))
    (fx', fy') = facePos newFace
    (cx', cy') = ((fx' * faceSize) + px', (fy' * faceSize) + py')
  in
    Pos heading' cx' cy'

move :: Board -> (Board -> Pos -> Pos) -> Move -> Pos -> Pos
move board advance m pos@(Pos heading x y) = case m of
  TurnLeft  -> Pos (turnLeft heading) x y
  TurnRight -> Pos (turnRight heading) x y
  Ahead 0   -> Pos heading x y
  Ahead n   ->
    let
      aheadPos = advance board pos
    in
      case boardAt board aheadPos of
        -- If the position ahead is empty, we move there and continue.
        '.' -> move board advance (Ahead $ n - 1) aheadPos
        -- If we hit a wall, we cannot move further and stay at the current pos.
        '#' -> move board advance (Ahead 0) pos
        _   -> error "Position should be inside the board."

password :: Pos -> Int
password (Pos heading x y) =
  let
    facing = case heading of
      Right -> 0
      Down  -> 1
      Left  -> 2
      Up    -> 3
    column = 1 + x
    row = 1 + y
  in
    1000 * row + 4 * column + facing

main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let
    (board, moves) = parseInput fileContents
    initialPos = initialPosition board
    finalPos1 = foldl' (flip $ move board advanceTorus) initialPos moves
    finalPos2 = foldl' (flip $ move board advanceCube) initialPos moves
  -- putStrLn $ show $ move board advanceCube (Ahead 1) (Pos Right (50*3-1) 0)
  -- putStrLn $ show $ move board advanceCube (Ahead 1) (Pos Up (50*3-1) 0)
  -- putStrLn $ show $ move board advanceCube (Ahead 1) (Pos Left (50*2) 0)
  -- putStrLn $ show $ move board advanceCube (Ahead 1) (Pos Down (50*2) (50-1))
  putStrLn $ "Part 1 answer: " <> (show $ password finalPos1)
  putStrLn $ "Part 2 answer: " <> (show $ password finalPos2)
