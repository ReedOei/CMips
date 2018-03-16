module LispCompiler where

import Control.Monad.State

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import LispLanguage
import CLanguage

data LispEnv = LispEnv [String] (Map String Type) (Map String (Map String Type))
    deriving Show

newEnvironment :: LispEnv
newEnvironment = LispEnv [] Map.empty Map.empty

-- The first type passed in should be the more specific one.
getMoreSpecificType :: Type -> Type -> Type
getMoreSpecificType (Type Pointer (NamedType "void")) t = t
getMoreSpecificType t (Type Pointer (NamedType "void")) = t
getMoreSpecificType (FunctionPointer aRetType aArgTypes) (FunctionPointer bRetType bArgTypes) =
    FunctionPointer (getMoreSpecificType aRetType bRetType) $ zipWith getMoreSpecificType aArgTypes bArgTypes
getMoreSpecificType a _ = a -- If we can't decide which is more specific, just get the first one.

updateFuncReturnType :: String -> Type -> State LispEnv ()
updateFuncReturnType funcName t = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Nothing -> put $ LispEnv names (Map.insert funcName t fTypes) vTypes
        Just curType -> put $ LispEnv names (Map.insert funcName (getMoreSpecificType t curType) fTypes) vTypes

updateVarType :: String -> String -> Type -> State LispEnv ()
updateVarType funcName varName t = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName vTypes of
        Nothing -> error $ "Trying to update variable type inside unknown function: '" ++ funcName ++ "'"
        Just vMap ->
            case Map.lookup varName vMap of
                Nothing -> put $ LispEnv names fTypes $ Map.insert funcName (Map.insert varName t vMap) vTypes
                Just curType -> put $ LispEnv names fTypes $ Map.insert funcName (Map.insert varName (getMoreSpecificType t curType) vMap) vTypes

getFuncType :: String -> String -> [Type] -> State LispEnv Type
getFuncType containingF funcName argTypes = do
    LispEnv _ fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Nothing -> do
            -- Try updating the variables because we could be referencing some function-valued variable
            updateVarType containingF funcName $ FunctionPointer (Type Pointer (NamedType "void")) argTypes
            getVarType containingF funcName

        Just t -> pure t

getOrInitFuncType :: String -> [Type] -> State LispEnv Type
getOrInitFuncType funcName argTypes = do
    LispEnv names fTypes vTypes <- get

    case Map.lookup funcName fTypes of
        Nothing -> do
            put $ LispEnv names (Map.insert funcName (Type Pointer (NamedType "void")) fTypes) vTypes
            getFuncType funcName funcName argTypes
        Just t -> pure t

getVarType :: String -> String -> State LispEnv Type
getVarType funcName varName = do
    LispEnv _ _ m <- get

    case Map.lookup varName =<< Map.lookup funcName m of
        Nothing -> error $ "Variable '" ++ varName ++ "' not found in function '" ++ funcName ++ "'" ++ show (Map.lookup funcName m)
        Just t -> pure t

getOrInitVarType :: String -> String -> State LispEnv Type
getOrInitVarType funcName varName = do
    LispEnv names fTypes m <- get

    case Map.lookup varName =<< Map.lookup funcName m of
        Nothing -> do
            -- Everything is a void pointer by default. yay!
            let newM = case Map.lookup funcName m of
                            Nothing -> Map.insert funcName (Map.insert varName (Type Pointer (NamedType "void")) Map.empty) m
                            Just varMap -> Map.insert funcName (Map.insert varName (Type Pointer (NamedType "void")) varMap) m

            put $ LispEnv names fTypes newM
            getVarType funcName varName
        Just t -> pure t

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
    funcBody <- concat <$> mapM (compileExpr fname) body

    types <- mapM (getOrInitVarType fname) args

    let go (_, Right st) = st
        go (_, Left expr) = ExprStatement expr

    let initial = map go $ init funcBody
    let (returnType, final) = case last funcBody of
                                (t, Left expr) -> (t, Return expr)
                                (t, Right st) -> (t, st)

    updateFuncReturnType fname returnType

    pure $ FuncDef returnType fname (zipWith Var types args) $ initial ++ [final]

compileExpr :: String -> Expr -> State LispEnv [(Type, Either CExpression CStatement)]
compileExpr fname (IntVal i) = pure [(NamedType "int", Left $ LitInt i)]
compileExpr fname (Quoted s) = pure [(Type Pointer (NamedType "char"), Left $ LitString s)]
compileExpr fname (Identifier i) = do
    t <- getOrInitVarType fname i
    pure [(t, Left $ VarRef i)]

compileExpr fname (Evaluate (Identifier "if") [condExpr,yes,no]) = do
    [(yesType, yesSt)] <- compileExpr fname yes

    let finalYes = case yesSt of
                    Left expr -> Return expr
                    Right st -> st

    [(noType, noSt)] <- compileExpr fname no

    let finalNo = case noSt of
                    Left expr -> Return expr
                    Right st -> st

    [(_, Left cond)] <- compileExpr fname condExpr

    pure [(yesType, Right $ IfStatement cond (Just (ElseBlock [finalNo])) [finalYes])]

compileExpr fname (Evaluate (Identifier called) args) = do
    compiled <- concat <$> mapM (compileExpr fname) args

    let argTypes@(t:_) = map fst compiled
    let exprs = map (\(_, Left expr) -> expr) compiled

    case find (\(name, op) -> name == called) cArithOps of
        Just (_, op) -> do
            let isBoolean = called `elem` cBooleanOps

            -- C booleans are integers.
            if isBoolean then
                pure [(NamedType "int", Left $ foldl1 (CBinaryOp op) exprs)]
            else
                pure [(t, Left $ foldl1 (CBinaryOp op) exprs)]
        Nothing -> do
            returnType <- case called of
                            "car" ->
                                case t of
                                    Type Pointer a -> pure a
                            "cdr" -> pure t
                            "empty" -> pure $ NamedType "int" -- Empty is a built-in that gives a bool
                            "cons" -> pure $ Type Pointer t -- Cons will build a list, so we will get a pointer.
                            name | name == fname -> getOrInitFuncType called argTypes
                            _ -> getFuncType fname called argTypes

            pure [(returnType, Left $ FuncCall called exprs)]

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

