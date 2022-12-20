#!/usr/bin/env runhaskell

module Main where

import Control.Monad (forM_)
import Data.List (foldl')

rotateAt :: Int -> [(Int, Int)] -> [(Int, Int)]
rotateAt i xs =
  let
    prefix = take i xs
    suffix = drop (i + 1) xs
    (j, n) = xs !! i
    np = length prefix
    ns = length suffix
    m  = if n >= 0 then n else ns + np + n + 1
    m' = if n > 0 then n - ns - 1 else np + n
  in
    case () of
      () | n >= 0 && n < ns -> prefix <> (take m suffix) <> [(j, n)] <> (drop m suffix)
      () | n > ns           -> (take m' prefix) <> [(j, n)] <> (drop m' prefix) <> suffix
      () | n < 0 && n > -np -> (take m' prefix) <> [(j, n)] <> (drop m' prefix) <> suffix
      () | otherwise        -> prefix <> (take m suffix) <> [(j, n)] <> (drop m suffix)

rotateNth :: Int -> [(Int, Int)] -> [(Int, Int)]
rotateNth n xs =
  let
    i = fst $ head $ filter (\(_, (j, _)) -> j == n) $ zip [0..] xs
  in
    rotateAt i xs

main :: IO ()
main = do
  fileLines <- lines <$> readFile "example.txt"
  let
    numbers :: [Int]
    numbers = fmap read fileLines
    withIndex = zip [0..] numbers
    indices = [0..((length numbers) - 1)]
    resultIndex = foldl' (flip rotateNth) withIndex indices
    result = fmap snd resultIndex

  putStrLn $ show result
