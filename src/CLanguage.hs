module CLanguage where

import Control.Lens ((^.), makeLenses)

import Data.Either (fromLeft, fromRight)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, maybeToList)

import Util

data VarKind = Pointer | Value
    deriving (Show, Eq)

data PreKind = Include
             | IfNDef
             | EndIf
             | CDefine
             | MiscPreKind
    deriving (Show, Eq)

data Type = NamedType String
          | Type VarKind Type
          | FunctionPointer Type [Type] -- Return type and argument types.
          | StructType CElement
          | Array Int Type -- An array of fixed size (like int arr[5];)
    deriving (Show, Eq)

data Var = Var [Annotation] Type String
    deriving (Show, Eq)

data CElement = Preprocessor PreKind String
                | FuncDef Type String [Var] [CStatement]
                | StructDef String [Var]
                | MiscElement
                | Inline Type String [Var] [String]
                | CommentElement CStatement
    deriving (Show, Eq)

data Annotation = Annotation String [String]
    deriving (Show, Eq)

data CStatement = Return (Maybe CExpression)
                | VarDef Var (Maybe CExpression)
                | IfStatement CExpression (Maybe CStatement) [CStatement]
                | ElseBlock [CStatement]
                | WhileStatement CExpression [CStatement]
                | ForStatement CStatement CExpression CStatement [CStatement]
                | Assign (Maybe BinaryOp) CExpression CExpression
                | ExprStatement CExpression
                | CComment String
                | Annotated [Annotation] (Maybe CStatement)
    deriving (Show, Eq)

data PrefixOp = PreIncrement
              | PreDecrement
              | PreNot
              | Dereference
              | AddressOf
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
                 | LitString String
                 | LitFloat Float
                 | NULL
                 | FuncCall String [CExpression]
                 | CPrefix PrefixOp CExpression
                 | CPostfix PostfixOp CExpression
                 | CArrayAccess CExpression CExpression
                 | CBinaryOp BinaryOp CExpression CExpression
    deriving (Show, Eq)

data CFile = CFile String [CElement]
    deriving (Show, Eq)

cArithOps = [("*", Mult), ("/", Div), ("%", Mod), ("+", Add), ("-", Minus),
             ("||", Or), ("&&", And),
             (">=", CGTE), ("<=", CLTE), ("!=", CNE), ("==", CEQ),
             ("|", OrBit), ("&", AndBit), ("<<", ShiftLeft), (">>", ShiftRight),
             ("^", Xor), (">", CGT), ("<", CLT)]
instance PrettyPrint BinaryOp where
    prettyPrint op = fromMaybe (error ("Unknown op: " ++ show op)) $
            lookup op $ map(\(a, b) -> (b, a)) cArithOps

cPrefixOps = [("++", PreIncrement), ("--", PreDecrement), ("!", PreNot), ("*", Dereference), ("&", AddressOf)]
instance PrettyPrint PrefixOp where
    prettyPrint op = fromMaybe (error ("Unknown op: " ++ show op)) $
            lookup op $ map(\(a, b) -> (b, a)) cPrefixOps

cPostfixOps = [("++", PostIncrement), ("--", PostDecrement)]
instance PrettyPrint PostfixOp where
    prettyPrint op = fromMaybe (error ("Unknown op: " ++ show op)) $
            lookup op $ map(\(a, b) -> (b, a)) cPostfixOps

instance PrettyPrint Annotation where
    prettyPrint (Annotation name properties) = "@" ++ name ++ "(" ++ intercalate "," properties ++ ")"

instance PrettyPrint PreKind where
    prettyPrint Include = "include"
    prettyPrint IfNDef = "ifndef"
    prettyPrint EndIf = "endif"
    prettyPrint CDefine = "define"
    prettyPrint MiscPreKind = ""

instance PrettyPrint CElement where
    prettyPrint (Preprocessor preKind str) = "#" ++ prettyPrint preKind ++ " " ++ str
    prettyPrint (FuncDef retType name args _) = prettyPrint retType ++ " " ++ name ++ "(" ++ intercalate "," (map prettyPrint args) ++ ");"
    prettyPrint (Inline retType name args _) = prettyPrint retType ++ " " ++ name ++ "(" ++ intercalate "," (map prettyPrint args) ++ ");"
    prettyPrint (StructDef name _) = "struct " ++ name
    prettyPrint (CommentElement (CComment str)) = "//" ++ str
    prettyPrint _ = ""

    prettyPrintLong (StructDef name vars) =
        "struct " ++ name ++ "{\n" ++
            intercalate "\n" (map ((++ ";") . ("    " ++ ) . prettyPrint) vars) ++ "\n" ++
        "};"
    prettyPrintLong (FuncDef retType name args body) =
        prettyPrint retType ++ " " ++ name ++ "(" ++ intercalate "," (map prettyPrint args) ++ ") {\n" ++
            intercalate "\n" (map (("    " ++) . prettyPrintLong) body) ++ "\n" ++
        "}"
    prettyPrintLong e = prettyPrint e

instance PrettyPrint CStatement where
    prettyPrint (ExprStatement expr) = prettyPrint expr ++ ";"
    prettyPrint (Return Nothing) = "return;"
    prettyPrint (Return (Just expr)) = "return " ++ prettyPrint expr ++ ";"
    prettyPrint (VarDef var Nothing) = prettyPrint var ++ ";"
    prettyPrint (VarDef var (Just ini)) = prettyPrint var ++ " = " ++ prettyPrint ini ++ ";"
    prettyPrint (IfStatement cond _ _) = "if (" ++ prettyPrint cond ++ ")"
    prettyPrint (ElseBlock _) = "else"
    prettyPrint (WhileStatement cond _) = "while (" ++ prettyPrint cond ++ ")"
    prettyPrint (ForStatement ini cond step _) = "for (" ++ prettyPrint ini ++ " " ++ prettyPrint cond ++ "; " ++ init (prettyPrint step) ++ ")"
    prettyPrint (Assign op accessExpr expr) = prettyPrint accessExpr ++ " " ++ maybeOp op ++ "= " ++ prettyPrint expr ++ ";"
    prettyPrint (Annotated annotations stmt) =
        case stmt of
            Nothing -> ""
            Just st -> prettyPrint st

    prettyPrintLong st@(ForStatement _ _ _ body) =
        prettyPrint st ++ " {\n" ++
            intercalate "\n" (map (("    " ++) . prettyPrintLong) body) ++ "\n" ++
        "}"
    prettyPrintLong st@(WhileStatement _ body) =
        prettyPrint st ++ " {\n" ++
            intercalate "\n" (map (("    " ++) . prettyPrintLong) body) ++ "\n" ++
        "}"
    prettyPrintLong st@(IfStatement _ elseBlock body) =
        prettyPrint st ++ " {\n" ++
            intercalate "\n" (map (("    " ++) . prettyPrintLong) body) ++ "\n" ++
        "} " ++ fromMaybe "" (prettyPrintLong <$> elseBlock)
    prettyPrintLong st@(ElseBlock body) =
        " " ++ prettyPrint st ++ " {\n" ++
            intercalate "\n" (map (("    " ++) . prettyPrintLong) body) ++ "\n" ++
        "}"
    prettyPrintLong (Annotated annotations stmt) =
        intercalate "\n" (map prettyPrint annotations) ++ "\n" ++
        case stmt of
            Nothing -> "."
            Just st -> prettyPrintLong st
    prettyPrintLong st = prettyPrint st

instance PrettyPrint CExpression where
    prettyPrint (LitInt n) = show n
    prettyPrint (LitString s) = show s
    prettyPrint (LitChar c) = show c
    prettyPrint (LitFloat f) = show f
    prettyPrint NULL = "NULL"
    prettyPrint (VarRef x) = x
    prettyPrint (CBinaryOp op a b) = "(" ++ prettyPrint a ++ " " ++ prettyPrint op ++ " " ++ prettyPrint b ++ ")"
    prettyPrint (FuncCall funcName args) = funcName ++ "(" ++ intercalate ", " (map prettyPrint args) ++ ")"
    prettyPrint (CPrefix Dereference expr) = "(" ++ prettyPrint Dereference ++ prettyPrint expr ++ ")"
    prettyPrint (CPrefix op expr) = prettyPrint op ++ prettyPrint expr
    prettyPrint (CPostfix op expr) = prettyPrint expr ++ prettyPrint op
    prettyPrint (CArrayAccess accessExpr expr) = prettyPrint accessExpr ++ "[" ++ prettyPrint expr ++ "]"
    prettyPrint (MemberAccess a b) = prettyPrint a ++ "." ++ prettyPrint b

instance PrettyPrint Type where
    prettyPrint (NamedType name) = name
    prettyPrint (Type Pointer t) = prettyPrint t ++ " *"
    prettyPrint (Type Value t) = prettyPrint t
    prettyPrint (FunctionPointer retType argTypes) = prettyPrint retType ++ "(" ++ intercalate "," (map prettyPrint argTypes) ++ ")"
    prettyPrint (Array arrSize t) = prettyPrint t ++ "[" ++ show arrSize ++ "]"
    prettyPrint (StructType (StructDef structName _)) = structName

instance PrettyPrint Var where
    prettyPrint (Var annotations t varName) = intercalate " " (map prettyPrint annotations) ++ prettyPrint t ++ " " ++ varName

instance PrettyPrint CFile where
    prettyPrint (CFile _ elements) = intercalate "\n\n" $ map prettyPrint elements

cBooleanOps = ["||", "&&", ">", "<", ">=", "<="]

maybeOp :: Maybe BinaryOp -> String
maybeOp Nothing = ""
maybeOp (Just op) = prettyPrint op

varType (Var _ t _) = t

