#!/usr/bin/env runhaskell

sum3 :: [Int] -> [Int]
sum3 xs = id
  -- Group the elements of lists with the first 0..2 elements dropped,
  -- then sum the groups.
  $ fmap sum
  $ foldr (\n -> zipWith (:) (drop n xs)) (repeat []) [0..2]

solve :: [Int] -> Int
solve xs =
  let
    xs3 = sum3 xs
  in
    length $ filter (\(x, y) -> y > x) (zip xs3 $ tail xs3) 

main :: IO ()
main = readFile "input.txt" >>= print . solve . fmap read . lines
