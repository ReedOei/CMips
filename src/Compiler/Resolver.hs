module Compiler.Resolver where

import Control.Monad.State

import Data.List (find)
import Data.Maybe (fromMaybe)

import CLanguage
import Compiler.Types

sizeof :: Type -> Int
sizeof (NamedType "char") = 1
sizeof (NamedType "unsigned char") = 1
sizeof (NamedType "short") = 2
sizeof (NamedType "long long int") = 8
sizeof (NamedType "double") = 8
sizeof (NamedType _) = 4
sizeof (Type Pointer _) = 4
sizeof _ = 4

findTypesElement :: CElement -> [Var]
findTypesElement (FuncDef _ _ vars statements) =
    vars ++ concatMap findTypesStatement statements
findTypesElement StructDef{} = []

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

-- Returns the offset of the member of the struct.
getStructOffset :: CExpression -> String -> State Environment Int
getStructOffset expr member = do
    StructType (StructDef _ members) <- resolveType expr >>= elaborateType

    let varTypes = map (\(Var varType _) -> varType) $ takeWhile (\(Var _ name) -> name /= member) members
    foldM (\n t -> do
                newT <- elaborateType t
                pure $ n + sizeof newT) 0 varTypes

resolveFuncCall :: String -> State Environment CElement
resolveFuncCall funcName = do
    Environment (CFile _ elements) _ _ <- get

    case findFuncCall funcName elements of
        Nothing -> error $ "Unresolved reference to function: " ++ funcName
        Just f -> pure f

elaborateType :: Type -> State Environment Type
elaborateType (NamedType structName) = do
    Environment (CFile _ elements) _ _ <- get
    case findStructDef structName elements of
        Nothing -> pure $ NamedType structName
        Just struct -> pure $ StructType struct
elaborateType (Type varKind t) = Type varKind <$> elaborateType t
elaborateType t = pure t

resolve :: String -> State Environment Var
resolve refName = do
    Environment (CFile _ elements) _ _ <- get
    pure $ fromMaybe (error ("Unknown reference to: " ++ refName)) $
                find (\(Var _ varName) -> varName == refName) $ concatMap findTypesElement elements

resolveType :: CExpression -> State Environment Type
resolveType (VarRef str) = resolve str >>= (\(Var varType _) -> pure varType)
resolveType (LitInt n) = pure $ Type Value $ NamedType "int"
resolveType (LitChar n) = pure $ Type Value $ NamedType "char"
resolveType NULL = pure $ Type Pointer $ NamedType "void"

resolveType (CPrefix Dereference expr) = do
    t <- resolveType expr

    case t of
        Type Pointer a -> pure a
        _ -> error $ "Cannot dereference non-pointer type '" ++ show t ++ "' in expression: " ++ show expr
resolveType (CPrefix _ expr) = resolveType expr

resolveType (CArrayAccess accessExpr _) = resolveType $ CPrefix Dereference accessExpr
resolveType (CPostfix _ expr) = resolveType expr
resolveType (FuncCall funcName _) = do
    FuncDef t _ _ _ <- resolveFuncCall funcName
    pure t
resolveType (MemberAccess accessExpr (VarRef name)) = do
    accessType <- resolveType accessExpr
    structType <- elaborateType accessType

    case structType of
        StructType (StructDef structName vars) ->
            case find (\(Var _ varName) -> varName == name) vars of
                Nothing -> error $ "Reference to struct '" ++ structName ++ "' member '" ++ name ++ "' undefined."
                Just (Var t _) -> pure t
        t -> error $ "Cannot access member of non-struct type: " ++ show t

resolveType expr@(CBinaryOp _ a b) = do
    aType <- resolveType a
    bType <- resolveType b

    if aType == bType then
        pure aType
    else
        error $ "Types in expression '" ++ readableExpr expr ++ "' don't match (" ++ show aType ++ " and " ++ show bType ++ ")"

