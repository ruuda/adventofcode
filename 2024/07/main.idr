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

data Expr
  = Const Nat
  | Add Expr Nat
  | Mul Expr Nat

Show Expr where
  show (Const n) = show n
  show (Add expr n) = (show expr) ++ " + " ++ (show n)
  show (Mul expr n) = (show expr) ++ " * " ++ (show n)

eval : Expr -> Nat
eval (Const n) = n
eval (Add expr n) = (eval expr) + n
eval (Mul expr n) = (eval expr) * n

-- Generate all possible expressions for the *reversed* terms.
exprs : List Nat -> List Expr
exprs []  = [Const 0]
exprs [n] = [Const n]
exprs (n :: more) = do
  rhs <- exprs more
  expr <- [Add rhs n, Mul rhs n]
  pure expr

mainPure : String -> Nat
mainPure str =
  let
    eqns = catMaybes $ map parseEqn $ lines str
    isGood : Eqn -> Bool
    isGood eqn = not
      $ null
      $ filter (\expr => eval expr == eqn.result)
      $ exprs
      $ reverse
      $ eqn.terms
    goodEqns = filter isGood eqns
  in
    foldr (+) 0 $ map (\x => x.result) $ goodEqns

main : IO ()
main = do
  readFile "input.txt" >>= \case
    Left err => putStrLn $ show err
    Right v => putStrLn $ show $ mainPure v
