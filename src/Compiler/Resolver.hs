module Compiler.Resolver where

import Control.Lens
import Control.Monad.State

import Data.List (find, maximumBy, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)

import CLanguage
import Compiler.Types
import MIPSLanguage
import Types
import Util

sizeof :: Type -> Int
sizeof (NamedType "char") = 1
sizeof (NamedType "unsigned char") = 1
sizeof (NamedType "short") = 2
sizeof (NamedType "long long int") = 8
sizeof (NamedType "double") = 8 -- Don't actually support doubles.
sizeof (NamedType "float") = 4
sizeof (NamedType _) = 4
sizeof (StructType (StructDef _ vars)) = sum $ map (\t -> max (sizeof t) maxSize) types
    where
        types = map varType vars
        maxSize = structPadding types
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
    types <- mapM (\(Var _ varType _) -> elaborateType varType) members
    let maxSize = structPadding types

    varTypes <- mapM (elaborateType . varType) $ takeWhile (\(Var _ _ name) -> name /= member) members
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

getCurrentFunction :: State Environment CElement
getCurrentFunction = do
    curFuncName <- view (global.curFunc) <$> get
    fromMaybe (error ("Currently in function '" ++ curFuncName ++ "' but this function does not seem to exist.")) <$>
        resolveFuncCall curFuncName

resolveFuncCall :: String -> State Environment (Maybe CElement)
resolveFuncCall funcName = do
    CFile _ elements <- view file <$> get

    pure $ findFuncCall funcName elements

-- | Resolves a function call by name for the current scope, and returns the return type/argument types.
resolveFuncType :: String -> State Environment (Maybe (Type, [Type]))
resolveFuncType funcName = do
    CFile _ elements <- view file <$> get

    case findFuncCall funcName elements of
        Just (FuncDef retType _ args _) ->
            Just <$> makeFuncType retType (map varType args)
        -- Possible function pointer, see if it's a local variable name.
        Nothing -> do
            def <- getCurrentFunction

            case find (fPointer funcName) $ getLocalVariables def of
                Nothing -> pure Nothing
                Just (Var _ (FunctionPointer retType argTypes) _) -> Just <$> makeFuncType retType argTypes
    where
        makeFuncType retType argTypes = (,) <$> elaborateType retType <*> mapM elaborateType argTypes

        fPointer name (Var _ (FunctionPointer _ _) varName) = name == varName
        fPointer _ _ = False

elaborateTypesVar :: Var -> State Environment Var
elaborateTypesVar (Var annotations varType varName) = Var <$> pure annotations <*> elaborateType varType <*> pure varName

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

resolve :: String -> State Environment Type
resolve "INT_MIN" = pure $ NamedType "int"
resolve "INT_MAX" = pure $ NamedType "int"
resolve "NULL" = pure $ Type Pointer $ NamedType "void"
resolve refName = do
    CFile _ elements <- view file <$> get

    curFuncName <- getCurFunc

    curFunc <- resolveFuncCall curFuncName

    case curFunc of
        Nothing -> error $ "Unknown current function '" ++ curFuncName ++ "' while trying to resolve '" ++ refName ++ "'"
        Just def ->
            case find (\(Var _ _ varName) -> varName == refName) $ getLocalVariables def of
                Just (Var _ varType _) -> pure varType
                -- Could be a function name.
                Nothing -> do
                    fType <- resolveFuncType refName

                    case fType of
                        Just (retType, argTypes) -> pure $ FunctionPointer retType argTypes
                        Nothing -> error $ "Unknown reference to ref: " ++ refName

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
resolveType (VarRef str) = resolve str
resolveType (LitInt n) = pure $ NamedType "int"
resolveType (LitChar n) = pure $ NamedType "char"
resolveType (LitFloat _) = pure $ NamedType "float"
resolveType NULL = pure $ Type Pointer $ NamedType "void"

resolveType (CPrefix Dereference expr) = do
    t <- resolveType expr >>= elaborateType

    case t of
        Type Pointer a -> pure a
        Array _ t -> pure t
        _ -> error $ "Cannot dereference non-pointer type '" ++ show t ++ "' in expression: " ++ show expr
resolveType (CPrefix _ expr) = resolveType expr >>= elaborateType

resolveType (CArrayAccess accessExpr _) = resolveType $ CPrefix Dereference accessExpr
resolveType (CPostfix _ expr) = resolveType expr >>= elaborateType
resolveType (FuncCall "malloc" _) = pure $ Type Pointer $ NamedType "void"
resolveType (FuncCall "printf" _) = pure $ NamedType "void"
resolveType (FuncCall funcName _) = do
    fType <- resolveFuncType funcName

    case fType of
        Nothing -> error $ "Undefined reference to function: '" ++ show funcName ++ "'"
        Just (retType, _) -> pure retType

resolveType (MemberAccess accessExpr (VarRef name)) = do
    accessType <- resolveType accessExpr
    structType <- elaborateType accessType

    let (structName, vars) =
            case structType of
                Type Value (StructType (StructDef structName vars)) -> (structName, vars)
                StructType (StructDef structName vars) -> (structName, vars)
                t -> error $ "Cannot access member of non-struct type: " ++ show t

    case find (\(Var _ _ varName) -> varName == name) vars of
        Nothing -> error $ "Reference to struct '" ++ structName ++ "' member '" ++ name ++ "' undefined."
        Just (Var _ t _) -> pure t

resolveType expr@(CBinaryOp _ a b) = do
    aType <- resolveType a
    bType <- resolveType b

    if aType == bType || (isNumericType aType && isNumericType bType) then
        pure $ getPriorityType aType bType
    else
        error $ "Types in expression '" ++ prettyPrint expr ++ "' don't match (" ++ show aType ++ " and " ++ show bType ++ ")"

resolveType expr = error $ "Unexpected expression type: " ++ show expr

isNumericType :: Type -> Bool
isNumericType (NamedType name) = "float" `isInfixOf` name || "int" `isInfixOf` name || "long" `isInfixOf` name || "char" `isInfixOf` name
isNumericType (Type Value t) = isNumericType t
isNumericType (Type Pointer t) = True
isNumericType _ = False

getPriorityType :: Type -> Type -> Type
getPriorityType t@(NamedType "float") _ = t
getPriorityType _ t@(NamedType "float") = t
getPriorityType t _ = t

isIntegral :: Type -> Bool
isIntegral (NamedType name) = "unsigned" `isInfixOf` name || "int" `isInfixOf` name || "long" `isInfixOf` name || "char" `isInfixOf` name
isIntegral _ = False

isFloating :: Type -> Bool
isFloating (NamedType "float") = True
isFloating _ = False

convertFloatToInt :: String -> String -> State Environment [MIPSInstruction]
convertFloatToInt source dest = do
    tempFloat <- useNextRegister "result_float" "temp_float_to_int"
    pure [Inst OP_MTC1 source tempFloat "",
          Inst OP_CVT_W_S tempFloat tempFloat "",
          Inst OP_MFC1 dest tempFloat ""]

convertIntToFloat :: String -> String -> State Environment [MIPSInstruction]
convertIntToFloat source dest = do
    tempFloat <- useNextRegister "result_float" "temp_int_to_float"
    pure [Inst OP_MTC1 source tempFloat "",
          Inst OP_CVT_S_W tempFloat tempFloat "",
          Inst OP_MFC1 dest tempFloat ""]

convert :: (String, Type) -> (String, Type) -> State Environment [MIPSInstruction]
convert (source, sourceType) (dest, destType)
    | isIntegral sourceType && isFloating destType = convertIntToFloat source dest
    | isIntegral destType && isFloating sourceType =  convertFloatToInt source dest
    | otherwise = pure [Inst OP_MOVE dest source ""]

