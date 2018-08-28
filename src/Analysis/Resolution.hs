module Analysis.Resolution where

import Control.Lens ((^.))

import CLanguage

-- | Will resolve an expression into a constant, if possible.
-- If successful, will be either a LitInt, LitChar, LitString, LitFloat, or NULL
resolveConstant :: Context CExpression -> Maybe CExpression
resolveConstant context =
    case context ^. val of
        LitInt i -> Just $ LitInt i
        LitChar c -> Just $ LitChar c
        LitString s -> Just $ LitString s
        LitFloat f -> Just $ LitFloat f
        NULL -> Just NULL
        _ -> Nothing

