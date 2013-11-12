#!/usr/bin/runhaskell

module Main where

import Data.Time
import Control.Monad

import Test.QuickCheck
import Text.ParserCombinators.Parsec

import SchemeParse (parseNumber)

testParser :: Parser a -> String -> (a -> Bool) -> Bool
testParser parser input predicate = case parse parser "" input of
    Left err -> False
    Right val -> predicate val

{-prop_parseNumber :: Integer -> Bool
prop_parseNumber = 

time :: IO DiffTime
time = liftM utctDayTime getCurrentTime

main :: IO ()
main = do startTime <- time
          tests
          endTime <- time
          putStrLn $ "Elapsed time: " ++ show (endTime - startTime)-}
