-- Build with "idris2 main.idr -o main".

module Main

import System.File.ReadWrite

mainPure : String -> Nat
mainPure lines = length lines

main : IO ()
main = do
  readFile "example.txt" >>= \case
    Left err => putStrLn $ show err
    Right v => putStrLn $ show $ mainPure v
