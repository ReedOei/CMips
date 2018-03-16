module LispLanguage where

import Data.List (intercalate)

import CLanguage

data MinilispFile = MinilispFile CFile [Expr]
    deriving Show

data Expr = IntVal Int |
            FloatVal Float |
            Identifier String |
            Quoted String |
            Evaluate Expr [Expr] |
            List [Expr] |
            Define String [String] [Expr] |
            Comment
    -- deriving Eq
    deriving (Eq, Show)

-- instance Show Expr where
--     show (Identifier s) = s
--     show (Quoted s) = show s
--     show (IntVal v) = show v
--     show (FloatVal fv) = show fv
--     show (List vals) = "(" ++ intercalate " " (map show vals) ++ ")"
--     show (Evaluate f vals) = "(" ++ show f ++ " " ++ intercalate " " (map show vals) ++ ")"
--     show (Define fname args body) = "(def " ++ fname ++ " (" ++ intercalate " " (map show args) ++ ") " ++ show body ++ ")"
--     show Comment = ""

