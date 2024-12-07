-- Build with "idris2 main.idr -o main".

module Main

import Data.List
import Data.List1
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
  -- Get the part before the colon.
  let (resStr, rem) = break (== ':') line
  res <- parsePositive resStr
  -- Cut off anything before the first digit
  let (_, termsStr) = break isDigit rem
  let terms = catMaybes $ forget $ map parsePositive $ split (== ' ') termsStr
  pure $ MkEqn res terms

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
