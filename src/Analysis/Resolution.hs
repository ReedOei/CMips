{-# LANGUAGE TupleSections #-}

module Analysis.Resolution where

import Control.Applicative
import Control.Lens ((^.), view)

import Data.List
import Data.Maybe

import Flow

import System.IO.Unsafe

import CLanguage
import Compiler.Context
import Types

isFuncDef :: String -> CElement -> Bool
isFuncDef funcName (FuncDef _ check _ _) = funcName == check
isFuncDef _ _ = False

contextFile :: Context t -> Maybe CFile
contextFile context = foldl (<|>) empty $ map isFile (context ^. parents)
    where
        isFile (FileParent file) = Just file
        isFile _ = Nothing

inFunction :: Enumerable t => Context a -> [Context t]
inFunction context =
    case (,) <$> contextFile context <*> context ^. funcName of
        Just (file@(CFile _ elements), fName) ->
            filter (isFuncDef fName) elements
            |> head
            |> defaultContext file
            |> enumElement
        Nothing -> []

findParent :: Parentable t => (Parent -> Maybe a) -> Context t -> Maybe (Context a)
findParent f context = contextualize v
    where
        newParents = dropWhile (isNothing . f) $ context ^. parents
        v = f =<< listToMaybe newParents
        -- Use drop 1 here in case newParents is empty, in which case tail would crash but drop 1 will return []
        contextualize p = makeContext context . ([],) <$> p

findAnnotation :: (Annotation -> Bool) -> Context t -> Maybe Annotation
findAnnotation f context = find f $ concatMap annotations $ context ^. parents
    where
        annotations (StatementParent (Annotated as _)) = as
        annotations _ = []

parentStatement :: Parentable t => Context t -> Maybe (Context CStatement)
parentStatement = findParent pred
    where
        pred (StatementParent st) = Just st
        pred _ = Nothing

references :: Parentable t => String -> Context t -> [Context CExpression]
references varName = filter reference' . enum
    where
        reference' context =
            case context ^. val of
                VarRef v -> v == varName
                _ -> False

bodyReferences :: String -> Context CStatement -> [Context CExpression]
bodyReferences varName context =
    concatMap (references varName) $ map (makeContext context) $
        case context ^. val of
            IfStatement cond elseBlock body ->
                map ([InvariantExpr cond],) $ body ++ maybe [] (\(ElseBlock ss) -> ss) elseBlock
            ElseBlock body -> map ([],) body
            WhileStatement cond body -> map ([InvariantExpr cond],) body
            ForStatement _ cond _ body -> map ([InvariantExpr cond],) body
            Annotated _ (Just st) -> [([], st)]
            _ -> []

containsReferences :: String -> Context CStatement -> Bool
containsReferences varName = not . null . bodyReferences varName

modifiesReference :: String -> Context CStatement -> Bool
modifiesReference varName = any modifyingStatement . mapMaybe parentStatement . bodyReferences varName
    where
        modifyingStatement context =
            case context ^. val of
                ExprStatement (CPrefix op (VarRef v))
                    | v == varName && op `elem` [PreIncrement, PreDecrement] -> True
                ExprStatement (CPostfix op (VarRef v))
                    | v == varName && op `elem` [PostIncrement, PostDecrement] -> True
                Assign _ (VarRef v) _ | v == varName -> True
                _ -> False

declaration :: String -> Context a -> Maybe (Context Var)
declaration decName = find declaration' . inFunction
    where
        declaration' context =
            let Var _ _ varName = context ^. val
            in varName == decName

-- | Will resolve an expression into a range of integers, if possible.
-- Pointers will be returned as integers, this will only happen if the possible value is null (TODO: Verify this is the case)
-- The returned list should never be empty (TODO: IS THIS TRUE?)
resolveIntConstant :: Context CExpression -> Maybe [Integer]
resolveIntConstant context =
    case context ^. val of
        LitInt i -> Just [fromIntegral i]
        -- LitChar c -> Just [c]
        -- LitString s -> Just [s]
        -- LitFloat f -> Just [f]
        NULL -> Just [0]
        _ -> Nothing

