module SchemeDef
    (LispVal ( Atom, List, DottedList, Number, String, Bool )) where

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
