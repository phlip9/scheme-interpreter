#!/usr/bin/runhaskell

module Main where

import System.Environment


printExpr :: String -> IO ()
printExpr input = print $ readExpr input

main :: IO ()
main = do args <- getArgs
          print $ readExpr $ head args
