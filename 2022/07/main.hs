#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad (foldM)
import Data.Map (Map)
import Prelude hiding (appendFile)
import Data.List (foldl', sortOn)

import qualified Data.Map as Map

data Dir = Dir
  { subdirs :: Map String Dir
  , files :: Map String Int
  } deriving (Show)

emptyDir = Dir Map.empty Map.empty

alter :: (Maybe Dir -> Maybe Dir) -> [String] -> Dir -> Dir
alter f path self = case path of
  [] -> case f (Just self) of
    Just dir -> dir
    Nothing  -> emptyDir
  part : more ->
    self { subdirs = Map.alter (fmap (alter f more)) part (subdirs self) }

showDir :: Dir -> [String]
showDir dir =
  let
    showFile (fname, size) = fname <> "\t(file, size=" <> (show size) <> ")"
    showSubdir (dirname, contents) = [dirname <> "\t(dir)"] <> (fmap ("  " <>) (showDir contents))
    fileLines = fmap showFile $ Map.toList $ files dir
    dirLines = concatMap showSubdir $ Map.toList $ subdirs dir
  in
    fileLines <> dirLines

data State = State 
  { statePath :: [String] 
  , stateRoot :: Dir
  } deriving (Show)

emptyState = State [] emptyDir

alterCurrent :: (Dir -> Dir) -> State -> State
alterCurrent f state =
  let
    f' Nothing    = Just $ f emptyDir
    f' (Just dir) = Just $ f dir
  in
    state { stateRoot = alter f' (statePath state) (stateRoot state) }

appendDir :: String -> Dir -> Dir
appendDir dirname dir = dir
  { subdirs = Map.insert dirname emptyDir (subdirs dir) }

appendFile :: String -> Int -> Dir -> Dir
appendFile fname size dir = dir
  { files = Map.insert fname size (files dir) }

execute :: String -> State -> State
execute cmd state = case words cmd of
  ["$", "cd", "/"]  -> state { statePath = [] }
  ["$", "cd", ".."] -> state { statePath = init $ statePath state }
  ["$", "cd", dir]  -> state { statePath = (statePath state) ++ [dir] }
  ["$", "ls"]       -> state
  ["dir", dirname]  -> alterCurrent (appendDir dirname) state
  [size, fname]     -> alterCurrent (appendFile fname (read size)) state

-- Traverse the tree and return all nodes, both internal and external.
allDirs :: Dir -> [Dir]
allDirs dir = dir : (concatMap allDirs $ Map.elems $ subdirs dir)

totalSize :: Dir -> Int
totalSize dir = sum
  $  (fmap totalSize $ Map.elems $ subdirs dir)
  <> (Map.elems $ files dir)

main :: IO ()
main = do
  fileContents <- lines <$> readFile "input.txt"
  let
    finalState = foldl' (\state line -> execute line state) emptyState fileContents
    smallDirs = filter (\dir -> totalSize dir < 100_000) $ allDirs $ stateRoot finalState

  putStrLn $ unlines $ showDir $ stateRoot finalState
  putStrLn $ "Part 1: " <> (show $ sum $ fmap totalSize smallDirs)

  let
    rootSize = totalSize $ stateRoot finalState
    diskSize = 70_000_000
    currentFree = diskSize - rootSize
    requiredToFree = 30_000_000 - currentFree
    candidates = id
      $ sortOn (\dir -> totalSize dir)
      $ filter (\dir -> totalSize dir >= requiredToFree)
      $ allDirs
      $ stateRoot finalState

  putStrLn $ "Num bytes to free: " <> (show requiredToFree)
  putStrLn $ "Part 2: " <> (show $ totalSize $ head candidates)
