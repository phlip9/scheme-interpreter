module SchemeParse
    ( parseAtom
    {-, parseString-}
    , parseNumber
    , parseExpr
    , readExpr) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import SchemeDef

{-spaces :: Parser ()
spaces = skipMany1 space-}

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

{-parseString :: Parser LispVal
parseString = do char '"'
                 x <- many chars
                 char '"'
                 return $ String x
    where chars = escapedChar <|> noneOf "\""
          escapedChar = char "\\" >> choice -}

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        {-<|> parseString-}
        <|> parseNumber

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
        Left err -> String $ "Parse error: " ++ show err
        Right val -> val
