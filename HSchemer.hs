#!/usr/bin/runhaskell

module Main where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
        show (Number n) = show n
        show (String s) = show s
        show (Bool _) | True = "#t"
                      | False = "#f"

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ noneOf "\""
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol 
               let atom = first : rest
               return $ case atom of
                             "#t" -> Bool True
                             "#f" -> Bool False
                             otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do ds <- many1 digit
                 return $ liftM (Number . read) ds
{-parseNumber = liftM (Number . read) $ many1 digit-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn $ readExpr $ head args          
