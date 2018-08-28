{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Analysis.Warnings where

import Control.Lens ((^.))

import CLanguage
import Util

data Warning where
    Warning :: forall t. PrettyPrint t => Context t -> String -> Warning

instance Show Warning where
    show (Warning context message) =
        case context ^. funcName of
            Nothing -> prettyPrint (context ^. val) ++ ": " ++ message
            Just fname -> fname ++ ": " ++ prettyPrint (context ^. val) ++ ": " ++ message

