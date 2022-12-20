#!/usr/bin/env runhaskell

module Main where

import Control.Monad (forM_, foldM_)
import Data.List (foldl')

rotateAt :: Int -> [(Int, Int)] -> [(Int, Int)]
rotateAt i xs =
  let
    prefix = take i xs
    suffix = drop (i + 1) xs
    (j, n) = xs !! i
    np = length prefix
    ns = length suffix
    -- NB: The puzzle's definition of moving backward is a bit weird, I would
    -- expect this:
    -- m  = if n >= 0 then n else ns + np + n + 1
    -- But when it wraps around, it makes one additional jump, so use -1 instead
    -- of -1.
    m  = if n >= 0 then n else ns + np + n
    m' = if n > 0 then n - ns else np + n
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

    indexOf0 = fst $ head $ filter (\(_i, n) -> n == 0) $ zip [0..] result
    elem1000 = result !! ((indexOf0 + 1000) `mod` (length numbers))
    elem2000 = result !! ((indexOf0 + 2000) `mod` (length numbers))
    elem3000 = result !! ((indexOf0 + 3000) `mod` (length numbers))
    answer = elem1000 + elem2000 + elem3000

  putStrLn $ show elem1000
  putStrLn $ show elem2000
  putStrLn $ show elem3000
  putStrLn $ show answer

  -- foldM_ (\xs i -> do
  --     putStrLn $ "Before " <> (show i) <> ": " <> show (fmap snd xs)
  --     pure $ rotateNth i xs
  --   ) withIndex indices

