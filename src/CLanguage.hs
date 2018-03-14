module CLanguage where

import Data.Either (fromLeft, fromRight)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

data VarKind = Pointer | Value
    deriving (Show, Eq)

data PreKind = Include
             | IfNDef
             | EndIf
             | Define
             | MiscPreKind
    deriving (Show, Eq)

data Type = NamedType String
          | Type VarKind Type
          | StructType CElement
    deriving (Show, Eq)

data Var = Var Type String
    deriving (Show, Eq)

data CElement = Preprocessor PreKind String
                | FuncDef Type String [Var] [CStatement]
                | StructDef String [Var]
                | MiscElement
    deriving (Show, Eq)

data CStatement = Return CExpression
                | VarDef Var (Maybe CExpression)
                | IfStatement CExpression (Maybe CStatement) [CStatement]
                | ElseBlock [CStatement]
                | WhileStatement CExpression [CStatement]
                | ForStatement CStatement CExpression CStatement [CStatement]
                | Assign (Maybe BinaryOp) CExpression CExpression
                | ExprStatement CExpression
                | CComment String
    deriving (Show, Eq)

data PrefixOp = PreIncrement
              | PreDecrement
              | PreNot
              | Dereference
    deriving (Show, Eq)

data PostfixOp = PostIncrement
               | PostDecrement
    deriving (Show, Eq)

data BinaryOp = Add | Minus | CGT | CLT | CGTE | CLTE | CNE | CEQ |
                Div | Mod | Xor | Or | And | OrBit | AndBit | ShiftLeft |
                ShiftRight | Mult
    deriving (Show, Eq)

data CExpression = VarRef String
                 | MemberAccess CExpression CExpression
                 | LitInt Int
                 | LitChar Char
                 | NULL
                 | FuncCall String [CExpression]
                 | CPrefix PrefixOp CExpression
                 | CPostfix PostfixOp CExpression
                 | CArrayAccess CExpression CExpression
                 | CBinaryOp BinaryOp CExpression CExpression
    deriving (Show, Eq)

data CFile = CFile String [CElement]
    deriving Show

cArithOps = [("*", Mult), ("/", Div), ("%", Mod), ("+", Add), ("-", Minus),
             ("||", Or), ("&&", And),
             (">=", CGTE), ("<=", CLTE), ("!=", CNE), ("==", CEQ),
             ("|", OrBit), ("&", AndBit), ("<<", ShiftLeft), (">>", ShiftRight),
             ("^", Xor), (">", CGT), ("<", CLT)]

cPostfixOps = [("++", PostIncrement), ("--", PostDecrement)]
cPrefixOps = [("++", PreIncrement), ("--", PreDecrement), ("!", PreNot), ("*", Dereference)]

readableOp :: BinaryOp -> String
readableOp op =
    fromMaybe (error ("Unknown op: " ++ show op)) $
        lookup op $ map(\(a, b) -> (b, a)) cArithOps

readablePrefix :: PrefixOp -> String
readablePrefix op =
    fromMaybe (error ("Unknown op: " ++ show op)) $
        lookup op $ map(\(a, b) -> (b, a)) cPrefixOps

readablePostfix :: PostfixOp -> String
readablePostfix op =
    fromMaybe (error ("Unknown op: " ++ show op)) $
        lookup op $ map(\(a, b) -> (b, a)) cPostfixOps

readableType :: Type -> String
readableType (NamedType name) = name
readableType (Type Pointer t) = readableType t ++ " *"
readableType (Type Value t) = readableType t

readableVar :: Var -> String
readableVar (Var t varName) = readableType t ++ " " ++ varName

maybeOp :: Maybe BinaryOp -> String
maybeOp Nothing = ""
maybeOp (Just op) = readableOp op

readable :: CStatement -> String
readable (ExprStatement expr) = readableExpr expr ++ ";"
readable (Return expr) = "return " ++ readableExpr expr ++ ";"
readable (VarDef var Nothing) = readableVar var ++ ";"
readable (VarDef var (Just ini)) = readableVar var ++ " = " ++ readableExpr ini ++ ";"
readable (IfStatement cond _ _) = "if (" ++ readableExpr cond ++ ")"
readable (ElseBlock _) = "else"
readable (WhileStatement cond _) = "while (" ++ readableExpr cond ++ ")"
readable (ForStatement ini cond step _) = "for (" ++ readable ini ++ " " ++ readableExpr cond ++ "; " ++ init (readable step) ++ ")"
readable (Assign op accessExpr expr) = readableExpr accessExpr ++ " " ++ maybeOp op ++ "= " ++ readableExpr expr ++ ";"

readableExpr :: CExpression -> String
readableExpr (LitInt n) = show n
readableExpr (LitChar c) = show c
readableExpr NULL = "NULL"
readableExpr (VarRef x) = x
readableExpr (CBinaryOp op a b) = "(" ++ readableExpr a ++ " " ++ readableOp op ++ " " ++ readableExpr b ++ ")"
readableExpr (FuncCall funcName args) = funcName ++ "(" ++ intercalate ", " (map readableExpr args) ++ ")"
readableExpr (CPrefix Dereference expr) = "(" ++ readablePrefix Dereference ++ readableExpr expr ++ ")"
readableExpr (CPrefix op expr) = readablePrefix op ++ readableExpr expr
readableExpr (CPostfix op expr) = readableExpr expr ++ readablePostfix op
readableExpr (CArrayAccess accessExpr expr) = readableExpr accessExpr ++ "[" ++ readableExpr expr ++ "]"
readableExpr (MemberAccess a b) = readableExpr a ++ "." ++ readableExpr b

