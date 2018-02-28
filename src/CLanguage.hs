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
                | Assign AssignKind String CExpression
    deriving (Show, Eq)

data CExpression = VarRef String
                 | LitInt Int
                 | CArrayAccess String CExpression
                 | CAdd CExpression CExpression
                 | CMinus CExpression CExpression
                 | CGT CExpression CExpression
                 | CLT CExpression CExpression
                 | CTestEq CExpression CExpression
                 | CLTE CExpression CExpression
                 | CGTE CExpression CExpression
                 | CNE CExpression CExpression
                 | CMult CExpression CExpression
                 | CDiv CExpression CExpression
                 | CMod CExpression CExpression
    deriving (Show, Eq)

data CFile = CFile String [CElement]
    deriving Show

