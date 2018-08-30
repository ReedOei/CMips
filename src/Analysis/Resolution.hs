{-# LANGUAGE FlexibleInstances #-}

module Analysis.Resolution where

import Control.Lens ((^.))

import Data.List

import CLanguage
import Compiler.Context

class Possible a

instance Possible Int where
instance Possible Char where
instance Possible String where
instance Possible Float where

findParent :: (Parent -> Bool) -> Context t -> Maybe Parent
findParent f context = find f $ context ^. parents

findAnnotation :: (Annotation -> Bool) -> Context t -> Maybe Annotation
findAnnotation f context = find f $ concatMap annotations $ context ^. parents
    where
        annotations (StatementParent (Annotated as _)) = as
        annotations _ = []

declaration :: String -> Context a -> Maybe (Context Var)
declaration decName context = do
    file <- contextFile context
    fName <- context ^. funcName

    find declaration' $ inFunction fName file

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

