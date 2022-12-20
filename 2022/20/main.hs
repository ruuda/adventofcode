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

main :: IO ()
main = do
  fileLines <- lines <$> readFile "input.txt"
  let
    numbers :: [Int]
    numbers = fmap read fileLines
    withIndex = zip [0..] numbers
    indices = [0..((length numbers) - 1)]
    resultIndex = foldl' (flip rotateNth) withIndex indices
    result = fmap snd resultIndex

    indexOf0 = fst $ head $ filter (\(_i, n) -> n == 0) $ zip [0..] result
    elem1000 = result !! ((indexOf0 + 1000) `mod` (length numbers))
    elem2000 = result !! ((indexOf0 + 2000) `mod` (length numbers))
    elem3000 = result !! ((indexOf0 + 3000) `mod` (length numbers))
    answer = elem1000 + elem2000 + elem3000

  -- foldM_ (\xs i -> do
  --     putStrLn $ "Before " <> (show i) <> ": " <> show (fmap snd xs)
  --     pure $ rotateNth i xs
  --   ) withIndex indices
  -- putStrLn $ "After: " <> (show result)

  putStrLn $ show elem1000
  putStrLn $ show elem2000
  putStrLn $ show elem3000
  putStrLn $ show answer
