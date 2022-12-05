#!/usr/bin/env runhaskell

module Main where

import Control.Monad (forM_)

main :: IO ()
main = do
  dataLines <- lines <$> readFile "example.txt"
  forM_ dataLines putStrLn
