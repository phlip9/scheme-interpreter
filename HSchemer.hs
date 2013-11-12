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
        show (Bool True) = "#t"
        show (Bool False) = "#f"

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol <|> char '#'
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _ -> Atom atom

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many chars
                 char '"'
                 return $ String x
    where chars = escapedChar <|> noneOf "\""
          escapedChar = char "\\" >> choice 

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
        Left err -> String $ "Parse error: " ++ show err
        Right val -> val

printExpr :: String -> IO ()
printExpr input = print $ readExpr input

main :: IO ()
main = do args <- getArgs
          print $ readExpr $ head args
