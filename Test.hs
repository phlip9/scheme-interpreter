#!/usr/bin/runhaskell

module Main where

import Test.QuickCheck

addOne :: Integer -> Integer
addOne num = num + 1

main :: IO ()
main = quickCheck (\c -> addOne c == c + 1)
