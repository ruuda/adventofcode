#!/usr/bin/env runhaskell

{-# LANGUAGE BangPatterns #-}

data Instr
  = Acc Int
  | Jmp Int
  | Nop Int

parseInstr :: String -> Instr
parseInstr s =
  let
    arg = read $ filter (/= '+') $ drop 4 s
  in
    case take 3 s of
      "acc" -> Acc arg
      "jmp" -> Jmp arg
      "nop" -> Nop arg

accumulatorBeforeFirstLoop :: [Instr] -> Int
accumulatorBeforeFirstLoop program = go 0 0 []
  where
    go !acc !pc !visited =
      case program !! pc of
        _ | pc `elem` visited -> acc
        Acc n -> go (acc + n) (pc + 1) (pc : visited)
        Jmp z -> go acc (pc + z) (pc : visited)
        Nop _ -> go acc (pc + 1) (pc : visited)

main :: IO ()
main = do
  program <- fmap parseInstr <$> lines <$> readFile "input.txt"
  putStrLn $ show $ accumulatorBeforeFirstLoop program
