module Compiler.Resolver where

import Control.Lens
import Control.Monad.State

import Data.List (find, maximumBy, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import CLanguage
import Compiler.Types

import System.IO.Unsafe

sizeof :: Type -> Int
sizeof (NamedType "char") = 1
sizeof (NamedType "unsigned char") = 1
sizeof (NamedType "short") = 2
sizeof (NamedType "long long int") = 8
sizeof (NamedType "double") = 8 -- Don't actually support doubles.
sizeof (NamedType "float") = 4
sizeof (NamedType _) = 4
sizeof (StructType (StructDef _ vars)) = sum $ map (sizeof . (\(Var varType _) -> varType)) vars
sizeof (Type Pointer _) = 4 -- Assume 32 bit pointers.
sizeof (Type Value t) = sizeof t
sizeof (Array size t) = size * sizeof t
sizeof _ = 4

findTypesElement :: CElement -> [Var]
findTypesElement (FuncDef _ _ vars statements) =
    vars ++ concatMap findTypesStatement statements
findTypesElement StructDef{} = []
findTypesElement _ = []

findTypesStatement :: CStatement -> [Var]
findTypesStatement (VarDef var _) = [var]
findTypesStatement (ForStatement ini _ _ block) = findTypesStatement ini ++ concatMap findTypesStatement block
findTypesStatement (IfStatement _ branch statements) = fromMaybe [] (findTypesStatement <$> branch) ++ concatMap findTypesStatement statements
findTypesStatement (ElseBlock statements) = concatMap findTypesStatement statements
findTypesStatement (WhileStatement _ statements) = concatMap findTypesStatement statements
findTypesStatement _ = []

findFuncCall :: String -> [CElement] -> Maybe CElement
findFuncCall _ [] = Nothing
findFuncCall funcName (f@(FuncDef _ checkName _ _):rest)
    | funcName == checkName = Just f
    | otherwise = findFuncCall funcName rest
findFuncCall funcName (_:rest) = findFuncCall funcName rest

findStructDef :: String -> [CElement] -> Maybe CElement
findStructDef _ [] = Nothing
findStructDef structName (s@(StructDef checkName _):rest)
    | structName == checkName = Just s
    | otherwise = findStructDef structName rest
findStructDef structName (_:rest) = findStructDef structName rest

getStructSizes :: Type -> Int
getStructSizes (Array arrSize t) = sizeof t
getStructSizes t = sizeof t

structPadding :: [Type] -> Int
structPadding types = maximum $ map getStructSizes types

structOffset :: CElement -> String -> State Environment Int
structOffset (StructDef _ members) member = do
    types <- mapM (\(Var varType _) -> elaborateType varType) members
    let maxSize = structPadding types

    let varTypes = map (\(Var varType _) -> varType) $ takeWhile (\(Var _ name) -> name /= member) members
    foldM (\n t -> do
                newT <- elaborateType t
                pure $ n + max maxSize (sizeof newT)) 0 varTypes

-- Returns the offset of the member of the struct.
getStructOffset :: CExpression -> String -> State Environment Int
getStructOffset expr member = do
    t <- resolveType expr >>= elaborateType

    let structDef =
            case t of
                Type Value (StructType s@(StructDef _ ms)) -> s
                StructType s@(StructDef _ ms) -> s
                _ -> error $ "Tried to get struct offset for member '" ++ member ++ "' but type is '" ++ show t ++ "' which is not a struct."

    structOffset structDef member

resolveFuncCall :: String -> State Environment (Maybe CElement)
resolveFuncCall funcName = do
    CFile _ elements <- view file <$> get

    pure $ findFuncCall funcName elements

elaborateTypesVar :: Var -> State Environment Var
elaborateTypesVar (Var varType varName) = Var <$> elaborateType varType <*> pure varName

elaborateTypes :: CElement -> State Environment CElement
elaborateTypes (FuncDef t funcName vars statements) =
    FuncDef <$> elaborateType t
            <*> pure funcName
            <*> mapM elaborateTypesVar vars
            <*> mapM elaborateTypesSt statements
elaborateTypes e = pure e

elaborateTypesSt :: CStatement -> State Environment CStatement
elaborateTypesSt (VarDef var expr) = VarDef <$> elaborateTypesVar var <*> pure expr
elaborateTypesSt (IfStatement cond (Just elseBlock) body) = IfStatement <$> pure cond <*> (Just <$> elaborateTypesSt elseBlock) <*> mapM elaborateTypesSt body
elaborateTypesSt (IfStatement cond Nothing body) = IfStatement <$> pure cond <*> pure Nothing <*> mapM elaborateTypesSt body
elaborateTypesSt (ElseBlock body) = ElseBlock <$> mapM elaborateTypesSt body
elaborateTypesSt (WhileStatement cond body) = WhileStatement <$> pure cond <*> mapM elaborateTypesSt body
elaborateTypesSt (ForStatement init cond step body) =
    ForStatement <$> elaborateTypesSt init
                 <*> pure cond
                 <*> elaborateTypesSt step
                 <*> mapM elaborateTypesSt body
elaborateTypesSt st = pure st

elaborateType :: Type -> State Environment Type
elaborateType (NamedType structName) = do
    CFile _ elements <- view file <$> get
    case findStructDef structName elements of
        Nothing -> pure $ NamedType structName
        Just struct -> pure $ StructType struct
elaborateType (Type Value t) = elaborateType t
elaborateType (Type varKind t) = Type varKind <$> elaborateType t
elaborateType t = pure t

resolve :: String -> State Environment Var
resolve refName = do
    CFile _ elements <- view file <$> get
    pure $ fromMaybe (error ("Unknown reference to: " ++ refName)) $
                find (\(Var _ varName) -> varName == refName) $ concatMap findTypesElement elements

-- Won't be 100% accurate for the scope, but it should be "good enough"
getVars :: CStatement -> [Var]
getVars (VarDef v _) = [v]
getVars (IfStatement _ (Just elseBlock) thenBlock) = getVars elseBlock ++ concatMap getVars thenBlock
getVars (IfStatement _ Nothing thenBlock) = concatMap getVars thenBlock
getVars (ElseBlock body) = concatMap getVars body
getVars (WhileStatement _ body) = concatMap getVars body
getVars (ForStatement init _ step body) = getVars init ++ getVars step ++ concatMap getVars body
getVars _ = []

getLocalVariables :: CElement -> [Var]
getLocalVariables (FuncDef retType name args body) = args ++ concatMap getVars body

resolveType :: CExpression -> State Environment Type
resolveType (VarRef str) = resolve str >>= (\(Var varType _) -> pure varType)
resolveType (LitInt n) = pure $ NamedType "int"
resolveType (LitChar n) = pure $ NamedType "char"
resolveType (LitFloat _) = pure $ NamedType "float"
resolveType NULL = pure $ Type Pointer $ NamedType "void"

resolveType (CPrefix Dereference expr) = do
    t <- resolveType expr

    case t of
        Type Pointer a -> pure a
        Array _ t -> pure t
        _ -> error $ "Cannot dereference non-pointer type '" ++ show t ++ "' in expression: " ++ show expr
resolveType (CPrefix _ expr) = resolveType expr

resolveType (CArrayAccess accessExpr _) = resolveType $ CPrefix Dereference accessExpr
resolveType (CPostfix _ expr) = resolveType expr
resolveType (FuncCall funcName _) = do
    f <- resolveFuncCall funcName

    case f of
        Just (FuncDef t _ _ _ ) -> pure t
        -- Possible function pointer, see if it's a local variable name.
        Nothing -> do
            curFuncName <- view (global.curFunc) <$> get
            curFuncDef <- resolveFuncCall curFuncName

            case curFuncDef of
                Nothing -> error $ "Currently in function '" ++ curFuncName ++ "' but this function does not seem to exist."
                Just def ->
                    case find (fPointer funcName) $ getLocalVariables def of
                        Nothing -> error $ "Undefined reference to function: '" ++ show funcName ++ "'"
                        Just (Var (FunctionPointer retType _) _) -> pure retType
    where
        fPointer name (Var (FunctionPointer _ _) varName) = name == varName
        fPointer _ _ = False

resolveType (MemberAccess accessExpr (VarRef name)) = do
    accessType <- resolveType accessExpr
    structType <- elaborateType accessType

    let (structName, vars) =
            case structType of
                Type Value (StructType (StructDef structName vars)) -> (structName, vars)
                StructType (StructDef structName vars) -> (structName, vars)
                t -> error $ "Cannot access member of non-struct type: " ++ show t

    case find (\(Var _ varName) -> varName == name) vars of
        Nothing -> error $ "Reference to struct '" ++ structName ++ "' member '" ++ name ++ "' undefined."
        Just (Var t _) -> pure t

resolveType expr@(CBinaryOp _ a b) = do
    aType <- resolveType a
    bType <- resolveType b

    if aType == bType || (isNumericType aType && isNumericType bType) then
        pure $ getPriorityType aType bType
    else
        error $ "Types in expression '" ++ readableExpr expr ++ "' don't match (" ++ show aType ++ " and " ++ show bType ++ ")"

resolveType expr = error $ "Unexpected expression type: " ++ show expr

isNumericType :: Type -> Bool
isNumericType (NamedType name) = "float" `isInfixOf` name || "int" `isInfixOf` name || "long" `isInfixOf` name || "char" `isInfixOf` name
isNumericType (Type Value t) = isNumericType t
isNumericType _ = False

getPriorityType :: Type -> Type -> Type
getPriorityType t@(NamedType "float") _ = t
getPriorityType _ t@(NamedType "float") = t
getPriorityType t _ = t

