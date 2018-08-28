module Analysis.Analyzer where

import Control.Lens ((^.))

import Analysis.Patterns
import Analysis.Resolution
import Analysis.Warnings

import CLanguage
import Compiler.Types

analyze :: CFile -> [Warning]
analyze file = [] -- analyzeArrayBounds file

analyzeArrayBounds :: CFile -> [Warning]
analyzeArrayBounds file = concatMap analyzeArrayBounds' matches
    where
        matches = allFileMatches arrayAccessPattern file
        analyzeArrayBounds' context =
            case context ^. val of
                CArrayAccess a b ->
                    case resolveConstant $ exprContext context b of
                        Just (LitInt i) ->
                            [Warning context "Array access with negative index!" | i < 0]
                        _ -> []
                _ -> []

