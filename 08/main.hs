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

data Result
  = Terminates Int
  | Loops Int
  deriving (Show)

-- Return the accumulator at the end of the program, or before it loops.
executeProgram :: [Instr] -> Result
executeProgram program = go 0 0 []
  where
    go !acc !pc !visited =
      case program !! pc of
        _ | pc `elem` visited -> Loops acc
        _ | pc == length program -> Terminates acc
        Acc n -> go (acc + n) (pc + 1) (pc : visited)
        Jmp z -> go acc (pc + z) (pc : visited)
        Nop _ -> go acc (pc + 1) (pc : visited)

flipAt :: [Instr] -> Int -> [Instr]
flipAt program i = fmap flipInstr $ zip [0..] program
  where
    flipInstr p = case p of
      (j, (Jmp z)) | i == j -> Nop z
      (j, (Nop z)) | i == j -> Jmp z
      (_, instr)            -> instr

flips :: [Instr] -> [[Instr]]
flips program = fmap (flipAt program) (take (length program) [0..])

firstTermination :: [Result] -> Result
firstTermination results = case results of
  (Loops _ : rs)     -> firstTermination rs
  (Terminates z : _) -> Terminates z

main :: IO ()
main = do
  program <- fmap parseInstr <$> lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> (show $ executeProgram program)
  putStrLn $ "Part 2: " <> (show $ firstTermination $ fmap executeProgram $ flips program)
