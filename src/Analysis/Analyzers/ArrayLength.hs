module Analysis.Analyzers.ArrayLength where

import Control.Applicative
import Control.Lens ((^.))

import Data.List

import Text.Read

import Analysis.Patterns
import Analysis.Resolution
import Analysis.Warnings

import CLanguage
import Compiler.Context

arrayAccessSearch :: CExpression -> Bool
arrayAccessSearch (CArrayAccess _ _) = True
arrayAccessSearch _ = False

arrayAccessPattern :: ExprPattern
arrayAccessPattern = ExprPattern "all array accesses" arrayAccessSearch

analyzeArrayBounds :: CFile -> [Warning]
analyzeArrayBounds file = concatMap analyzeArrayBounds' matches
    where
        matches = allFileMatches arrayAccessPattern file
        analyzeArrayBounds' context =
            case context ^. val of
                CArrayAccess a b ->
                    [Warning context (show (arrayLength context))] ++
                    case resolveIntConstant $ makeContext context ([], b) of
                        Just xs ->
                            [Warning context "Array access with a possibly negative index!" | any (< 0) xs]
                        _ -> []
                _ -> []

-- | Takes in a context of an array access and attempts to resolve the length of the array.
-- If successful, will either return the variable containing the length or the actual length, if possible
arrayLength :: Context CExpression -> Maybe (Either String Int)
arrayLength context =
    case context ^. val of
        CArrayAccess (VarRef varName) _ ->
            arrayLengthAnnotation context varName <|>
            -- arrayLengthContext context varName <|>
            empty
        _ -> Nothing

arrayLengthAnnotation :: Context CExpression -> String -> Maybe (Either String Int)
arrayLengthAnnotation context varName =
    case find lengthAnnotation =<< (annotations <$> declaration varName context) of
        Nothing -> Nothing
        Just (Annotation _ [lengthVal]) ->
            Right <$> readMaybe lengthVal <|>
            Left <$> Just lengthVal
    where
        lengthAnnotation (Annotation "Length" _) = True
        lengthAnnotation _ = False

        annotations context = let Var as _ _ = context ^. val in as

