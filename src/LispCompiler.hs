{-# LANGUAGE TupleSections #-}

module LispCompiler where

import Control.Monad.State

import Data.Either (either)
import Data.List (find, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)

import LispLanguage
import CLanguage

import System.IO.Unsafe

data LispEnv = LispEnv [String] (Map String Type) (Map String (Map String Type))
    deriving Show

newEnvironment :: LispEnv
newEnvironment = LispEnv [] Map.empty Map.empty

defaultType = Type Pointer (NamedType "void")
defaultFuncType n = FunctionPointer defaultType $ replicate n defaultType

-- The first type passed in should be the more specific one.
getMoreSpecificType :: Type -> Type -> Type
getMoreSpecificType (Type Pointer (NamedType "void")) t = t
getMoreSpecificType t (Type Pointer (NamedType "void")) = t
getMoreSpecificType (FunctionPointer aRetType aArgTypes) (FunctionPointer bRetType bArgTypes) =
    FunctionPointer (getMoreSpecificType aRetType bRetType) $ zipWith getMoreSpecificType aArgTypes bArgTypes
getMoreSpecificType a _ = a -- If we can't decide which is more specific, just get the first one.

useNextId :: String -> State LispEnv String
useNextId requested = do
    LispEnv ids fTypes vTypes <- get

    let found = filter (requested `isPrefixOf`) ids
    let newid = requested ++ show (length found)
    put $ LispEnv (newid : ids) fTypes vTypes
    pure newid

-- Can only update return types for function pointers
updateRetType :: Type -> Type -> Type
updateRetType newRetType (FunctionPointer retType argTypes) = FunctionPointer (getMoreSpecificType newRetType retType) argTypes

updateFuncReturnType :: String -> Type -> State LispEnv ()
updateFuncReturnType funcName t = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Just _ -> put $ LispEnv names (Map.update (Just . updateRetType t) funcName fTypes) vTypes

updateVarType :: String -> String -> Type -> State LispEnv ()
updateVarType funcName varName t = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName vTypes of
        Nothing -> error $ "Trying to update variable '" ++ varName ++ "' inside unknown function: '" ++ funcName ++ "'"
        Just vMap ->
            case Map.lookup varName vMap of
                Nothing -> put $ LispEnv names fTypes $ Map.insert funcName (Map.insert varName t vMap) vTypes
                Just curType -> put $ LispEnv names fTypes $ Map.insert funcName (Map.insert varName (getMoreSpecificType t curType) vMap) vTypes

getFuncArgTypes :: String -> State LispEnv [Type]
getFuncArgTypes "car" = pure [Type Pointer (NamedType "List")]
getFuncArgTypes "cdr" = pure [Type Pointer (NamedType "List")]
getFuncArgTypes "empty" = pure [Type Pointer (NamedType "List")]
getFuncArgTypes "cons" = pure [defaultType, Type Pointer (NamedType "List")]
getFuncArgTypes "printf" = pure [Type Pointer (NamedType "char")]
getFuncArgTypes funcName = do
    LispEnv _ fTypes _ <- get

    case Map.lookup funcName fTypes of
        Just (FunctionPointer _ argTypes) -> pure argTypes
        Nothing -> error $ "Error: function not found: '" ++ funcName ++ "'"

getFuncVarType :: String -> String -> [Type] -> State LispEnv Type
getFuncVarType containingF funcName argTypes = do
    -- Try updating the variables because we could be referencing some function-valued variable
    updateVarType containingF funcName $ FunctionPointer (Type Pointer (NamedType "void")) argTypes
    getVarType containingF funcName

getFuncType :: String -> State LispEnv (Maybe Type)
getFuncType funcName = do
    LispEnv _ fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Nothing -> pure Nothing
        Just t -> pure $ Just t

getOrInitFuncType :: String -> [Type] -> State LispEnv Type
getOrInitFuncType funcName argTypes = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Nothing -> do
            let t = FunctionPointer defaultType argTypes
            put $ LispEnv names (Map.insert funcName t fTypes) vTypes
            pure t
        Just t -> pure t

getVarType :: String -> String -> State LispEnv Type
getVarType funcName varName = do
    LispEnv _ _ m <- get

    case Map.lookup varName =<< Map.lookup funcName m of
        Nothing -> error $ "Variable '" ++ varName ++ "' not found in function '" ++ funcName ++ "'"
        Just t -> pure t

getOrInitVarType :: String -> String -> State LispEnv Type
getOrInitVarType funcName varName = do
    LispEnv names fTypes m <- get

    case Map.lookup varName =<< Map.lookup funcName m of
        Nothing -> do
            let newM = case Map.lookup funcName m of
                            Nothing -> Map.insert funcName (Map.insert varName defaultType Map.empty) m
                            Just varMap -> Map.insert funcName (Map.insert varName defaultType varMap) m

            put $ LispEnv names fTypes newM
            getVarType funcName varName
        Just t -> pure t

getArgCount :: String -> String -> State LispEnv Int
getArgCount funcName varName = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Just (FunctionPointer _ argTypes) -> pure $ length argTypes
        Nothing ->
            case Map.lookup varName =<< Map.lookup funcName vTypes of
                Nothing -> error $ "Variable '" ++ varName ++ "' not found in function '" ++ funcName ++ "'"
                Just (FunctionPointer _ argTypes) -> pure $ length argTypes


compileLisp :: MinilispFile -> CFile
compileLisp (MinilispFile (CFile _ elements) exprs) = CFile "" $ elements ++ compiled
    where
        (compiled, _) = runState doCompile newEnvironment
        doCompile = foldM go [] exprs
        go cur expr = do
            element <- compileDef expr
            pure $ cur ++ [element]

compileDef :: Expr -> State LispEnv CElement
compileDef (Define fname args body) = do
    let newBody = foldl doRep body $ zip args [0..]

    funcBody <- concat <$> mapM (compileExpr fname) newBody

    types <- mapM (getOrInitVarType fname) args

    let go (_, Right st) = st
        go (_, Left expr) = ExprStatement expr

    let initial = map go $ init funcBody
    let (returnType, final) = case last funcBody of
                                (t, Left expr) -> (t, Return expr)
                                (t, Right st) -> (t, st)

    getOrInitFuncType fname types
    updateFuncReturnType fname returnType

    pure $ FuncDef returnType fname [Var (Type Pointer (NamedType "List")) "args"] $ initial ++ [final]
    where
        doRep curBody (arg, i) =
            let call = Evaluate (Identifier "car") [iterate (\prev -> Evaluate (Identifier "cdr") [prev]) (Identifier "args") !! i] in
                map (replaceInExpr arg call) curBody

replaceInExpr :: String -> Expr -> Expr -> Expr
replaceInExpr name replacement (Identifier iName)
    | name == iName = replacement
    | otherwise = Identifier iName
replaceInExpr name replacement (Evaluate a args) = Evaluate (replaceInExpr name replacement a) $ map (replaceInExpr name replacement) args
replaceInExpr name replacement (List vals) = List $ map (replaceInExpr name replacement) vals
replaceInExpr _ _ expr = expr

compileExpr :: String -> Expr -> State LispEnv [(Type, Either CExpression CStatement)]
compileExpr fname (IntVal i) = pure [(NamedType "int", Left $ LitInt i)]
compileExpr fname (Quoted s) = pure [(Type Pointer (NamedType "char"), Left $ LitString s)]
compileExpr fname (Identifier i) = do
    t <- getFuncType i
    case t of
        Nothing -> do
            t <- getOrInitVarType fname i
            pure [(t, Left $ VarRef i)]
        Just FunctionPointer{} -> compileExpr fname (Evaluate (Identifier i) [])

compileExpr fname (Evaluate (Identifier "if") [condExpr,yes,no]) = do
    yesSts <- compileExpr fname yes
    let (yesType, finalYes) = case last yesSts of
                                (t, Left expr) -> (t, Return expr)
                                (t, Right st) -> (t, st)

    noSts <- compileExpr fname no
    let (_, finalNo) = case last noSts of
                            (t, Left expr) -> (t, Return expr)
                            (t, Right st) -> (t, st)

    condSts <- compileExpr fname condExpr

    let condInit = init condSts
    let (_, Left cond) = last condSts
    let thenBlock = map (either ExprStatement id . snd) (init yesSts) ++ [finalYes]
    let elseBlock = map (either ExprStatement id . snd) (init noSts) ++ [finalNo]

    pure $ condInit ++ [(yesType, Right $ IfStatement cond (Just (ElseBlock elseBlock)) thenBlock)]

compileExpr fname (Evaluate (Identifier called) args) = do
    compiled <- mapM (compileExpr fname) args

    let argTypes = map (fst . last) compiled
    let exprs = map ((\(_, Left expr) -> expr) . last) compiled

    let initializationSts = concatMap init compiled

    if isStdLibraryFunction called then
        handleStdFunction fname $ Evaluate (Identifier called) args
    else do
        case find (\(name, op) -> name == called) cArithOps of
            Just (_, op) -> do
                let isBoolean = called `elem` cBooleanOps

                let (t:_) = argTypes

                -- C booleans are integers.
                if isBoolean then
                    pure $ initializationSts ++ [(NamedType "int", Left $ foldl1 (CBinaryOp op) exprs)]
                else
                    pure $ initializationSts ++ [(t, Left $ foldl1 (CBinaryOp op) exprs)]
            Nothing -> do
                returnType <-
                    case called of
                        name | name == fname -> getOrInitFuncType called argTypes -- handle recursion.
                        _ -> fromJust <$> getFuncType called

                loadName <- useNextId "temp_exec_f"
                argCountName <- useNextId "temp_exec_f_argcount"
                argsName <- useNextId "temp_exec_f_args"

                let loadSt = VarDef (Var (FunctionPointer defaultType argTypes) loadName) $ Just $ VarRef called

                argCount <- length <$> getFuncArgTypes called
                let argCountSt = VarDef (Var (NamedType "int") argCountName) $ Just $ LitInt argCount

                let argList = foldl (\cur newArg -> FuncCall "cons" [newArg, cur]) NULL $ reverse exprs
                let argSt = VarDef (Var (Type Pointer (NamedType "List")) argsName) $ Just argList
                let callSeq = map (\st -> (NamedType "void", Right st)) [loadSt, argCountSt, argSt]

                if length exprs == argCount then -- Just execute it.
                    pure $ initializationSts ++ [(returnType, Left $ FuncCall called [argList])]
                else do -- partially apply
                    let resSt = FuncCall "apply" [FuncCall "cons" [VarRef loadName, FuncCall "cons" [VarRef argCountName, VarRef argsName]]]
                    pure $ initializationSts ++ callSeq ++ [(returnType, Left resSt)]

compileExpr fname (Evaluate expr args) = do
    compiled <- mapM (compileExpr fname) args
    compiledExpr <- compileExpr fname expr

    let argTypes@(t:_) = map (fst . last) compiled
    let exprs = map ((\(_, Left expr) -> expr) . last) compiled

    let initializationSts = concatMap init compiled

    let returnType = FunctionPointer defaultType argTypes

    argsName <- useNextId "temp_exec_f_args"
    resultName <- useNextId "temp_exec_f_name"

    let initExpr = init compiledExpr
    let (_, Left finalExpr) = last compiledExpr
    let loadFSt = VarDef (Var defaultType resultName) $ Just finalExpr

    let argList = foldl (\cur newArg -> FuncCall "append" [newArg, cur]) (VarRef resultName) exprs
    let argSt = VarDef (Var (Type Pointer (NamedType "List")) argsName) $ Just argList

    let resSt = FuncCall "apply" [VarRef argsName]

    pure $ initializationSts ++ initExpr ++ [(NamedType "void", Right loadFSt), (NamedType "void", Right argSt)] ++ [(returnType, Left resSt)]

compileExpr fname (List vals) = do
    compiled <- concat <$> mapM (compileExpr fname) (reverse vals)

    case compiled of
        [] -> pure [(Type Pointer (NamedType "List"), Left NULL)]
        _ -> do
            let types@(t:_) = map fst compiled
            let exprs = map (\(_, Left expr) -> expr) compiled

            let result = foldl go NULL $ zip types exprs

            pure [(Type Pointer (NamedType "List"), Left result)]
    where
        go prev (t, LitInt i) =
            FuncCall "cons" [LitInt i, prev]

handleStdFunction fname (Evaluate (Identifier called) args) = do
    compiled <- mapM (compileExpr fname) args

    let argTypes@(t:_) = map (fst . last) compiled
    let exprs = map ((\(_, Left expr) -> expr) . last) compiled

    let initializationSts = concatMap init compiled
    let resSt = FuncCall called exprs

    let retType = defaultType

    pure $ initializationSts ++ [(retType, Left resSt)]

isStdLibraryFunction :: String -> Bool
isStdLibraryFunction name = name `elem` ["car", "cdr", "empty", "append", "cons", "printf", "length", "apply"]

