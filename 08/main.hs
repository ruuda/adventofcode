#!/usr/bin/env runhaskell

{-# LANGUAGE BangPatterns #-}

import Data.IntSet (IntSet)
import Data.Vector (Vector)

import qualified Data.IntSet as IntSet
import qualified Data.Vector as Vector

data Instr
  = Acc Int
  | Jmp Int
  | Nop Int

parseInstr :: String -> Instr
parseInstr s =
  let
    arg = read $ fitler (/= '+') $ drop 4 s
  in
    case take 3 s of
      "acc" -> Acc arg
      "jmp" -> Jmp arg
      "nop" -> Nop arg

accumulatorBeforeFirstLoop :: Vector Instr -> Int
accumulatorBeforeFirstLoop program = go 0 0 IntSet.empty
  where
    go !acc !pc !visited =
      let
        visited' = IntSet.insert pc visited
      in
        case program Vector.! pc of
          _ | IntSet.member pc visited -> acc
          Acc n -> go (acc + n) (pc + 1) visited'
          Jmp z -> go acc z visited'
          Nop   -> go acc (pc + 1) visited'

main :: IO ()
main = do
  program <- Vector.fromList $ lines <$> readFile "input.txt"
  putStrLn $ show $ accumulatorBeforeFirstLoop program
