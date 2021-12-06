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

-- An alternative definition of "solve" that is a bit more low-level but also
-- less cryptic and a bit shorter depending on how you count.
solveAlt :: [Int] -> Int
solveAlt = solveAlt' 0
  where
    solveAlt' n (a:b:c:d:more) | a + b + c < b + c + d = solveAlt' (n + 1) (b:c:d:more)
    solveAlt' n (_:b:c:d:more) | otherwise             = solveAlt' n       (b:c:d:more)
    solveAlt' n _ = n

main :: IO ()
main = readFile "input.txt" >>= print . solve . fmap read . lines
