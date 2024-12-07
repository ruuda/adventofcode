-- Build with "idris2 main.idr -o main".

module Main

import Data.List
import Data.String
import Debug.Trace
import System.File.ReadWrite

-- An equation in the input, with result and terms.
record Eqn where
  constructor MkEqn
  result: Nat
  terms: List Nat

Show Eqn where
  show eqn = (show eqn.result) ++ " = " ++ (unwords $ map show $ eqn.terms)

parseEqn : String -> Maybe Eqn
parseEqn line = do
  let (resStr, rem) = break (== ':') line
  res <- parsePositive resStr
  pure $ MkEqn res []

mainPure : String -> Nat
mainPure str =
  let
    eqns = traceVal $ catMaybes $ map parseEqn $ lines str
  in
    foldr (+) 0 $ map (\x => x.result) eqns

main : IO ()
main = do
  readFile "example.txt" >>= \case
    Left err => putStrLn $ show err
    Right v => putStrLn $ show $ mainPure v
