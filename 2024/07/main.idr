-- Build with "idris2 main.idr -o main".

module Main

import Data.List
import Data.List1
import Data.Nat
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
  | Cat Expr Nat

Show Expr where
  show (Const n) = show n
  show (Add expr n) = (show expr) ++ " + " ++ (show n)
  show (Mul expr n) = (show expr) ++ " * " ++ (show n)
  show (Cat expr n) = (show expr) ++ " || " ++ (show n)

eval : Expr -> Nat
eval (Const n) = n
eval (Add expr n) = (eval expr) + n
eval (Mul expr n) = (eval expr) * n
eval (Cat expr n) = (power 10 (length $ show n)) * (eval expr) + n

data Part = Part1 | Part2

-- Generate all possible expressions for the *reversed* terms.
exprs : Part -> List Nat -> List Expr
exprs _ []  = [Const 0]
exprs _ [n] = [Const n]
exprs Part1 (n :: more) = do
  rhs <- exprs Part1 more
  expr <- [Add rhs n, Mul rhs n]
  pure expr
exprs Part2 (n :: more) = do
  rhs <- exprs Part2 more
  expr <- [Add rhs n, Mul rhs n, Cat rhs n]
  pure expr

mainPure : String -> List Nat
mainPure str =
  let
    eqns = catMaybes $ map parseEqn $ lines str
    isGood : Part -> Eqn -> Bool
    isGood part eqn = not
      $ null
      $ filter (\expr => eval expr == eqn.result)
      $ exprs part
      $ reverse
      $ eqn.terms
    answer : Part -> Nat
    answer part =
      foldr (+) 0
      $ map (\eqn => eqn.result)
      $ filter (isGood part) eqns
  in
    [answer Part1, answer Part2]

main : IO ()
main = do
  readFile "input.txt" >>= \case
    Left err => putStrLn $ show err
    Right v => putStrLn $ show $ mainPure v
