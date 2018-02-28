module CLanguage where

data VarKind = Pointer | Value
    deriving (Show, Eq)

data PreKind = Include
             | IfNDef
             | EndIf
             | Define
             | MiscPreKind
    deriving (Show, Eq)

data AssignKind = AssignNormal
                | AssignAdd
                | AssignOr
                | AssignAnd
                | AssignMinus
                | AssignMult
                | AssignDiv
                | AssignXor
    deriving (Show, Eq)

data Type = Type Bool VarKind String
    deriving (Show, Eq)

data Var = Var Type String
    deriving (Show, Eq)

data CElement = Preprocessor PreKind String
                | FuncDef Type String [Var] Bool [CStatement]
                | MiscElement
    deriving (Show, Eq)

data CStatement = Return CExpression
                | VarDef Var (Maybe CExpression)
                | IfStatement CExpression [CStatement]
                | WhileStatement CExpression [CStatement]
                | ForStatement CStatement CExpression CExpression [CStatement]
                | Assign AssignKind String CExpression
                | ExprStatement CExpression
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
                 | LitInt Int
                 | FuncCall String [CExpression]
                 | CPrefix PrefixOp CExpression
                 | CPostfix PostfixOp CExpression
                 | CArrayAccess String CExpression
                 | CBinaryOp BinaryOp CExpression CExpression
    deriving (Show, Eq)

data CFile = CFile String [CElement]
    deriving Show

