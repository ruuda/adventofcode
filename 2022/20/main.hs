#!/usr/bin/env runhaskell

module Main where

import Control.Exception (assert)
import Control.Monad (forM_, foldM_)
import Data.List (foldl')

-- Insert element `x` at so it is at index `i` in the result.
insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs
  = assert (i >= 0)
  $ assert (i <= length xs)
  $ (take i xs) <> [x] <> (drop i xs)

rotateAt :: Int -> [(Int, Int)] -> [(Int, Int)]
rotateAt i xs =
  let
    prefix = take i xs
    suffix = drop (i + 1) xs
    (j, n) = xs !! i
    newIndex = (i + n) `mod` (length xs - 1)
  in
    insertAt newIndex (j, n) (prefix <> suffix)

rotateNth :: Int -> [(Int, Int)] -> [(Int, Int)]
rotateNth n xs =
  let
    i = fst $ head $ filter (\(_, (j, _)) -> j == n) $ zip [0..] xs
  in
    rotateAt i xs

mix :: [(Int, Int)] -> [(Int, Int)]
mix xs = foldl' (flip rotateNth) xs [0..((length xs) - 1)]

extractAnswer :: [Int] -> Int
extractAnswer xs =
  let
    indexOf0 = fst $ head $ filter (\(_i, n) -> n == 0) $ zip [0..] xs
    elem1000 = xs !! ((indexOf0 + 1000) `mod` (length xs))
    elem2000 = xs !! ((indexOf0 + 2000) `mod` (length xs))
    elem3000 = xs !! ((indexOf0 + 3000) `mod` (length xs))
  in
    elem1000 + elem2000 + elem3000

-- Also in GHC.Utils.Misc, but I can't import from there so let's define it
-- ourselves.
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

main :: IO ()
main = do
  fileLines <- lines <$> readFile "input.txt"
  let
    numbers :: [Int]
    numbers = fmap read fileLines
    withIndex1 = zip [0..] numbers
    withIndex2 = zip [0..] (fmap (* 811589153) numbers)
    result1 = fmap snd $ mix withIndex1
    result2 = fmap snd $ (nTimes 10 mix) withIndex2

  putStrLn $ "Part 1: " <> (show $ extractAnswer result1)
  putStrLn $ "Part 2: " <> (show $ extractAnswer result2)
