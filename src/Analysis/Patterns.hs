{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Analysis.Patterns where

import Control.Lens (view)

import Data.List
import Data.Maybe

import CLanguage
import Compiler.Context
import Util

class (Show a, Enumerable b) => Matchable a b where
    match :: a -> b -> Bool

    allFileMatches :: a -> CFile -> [Context b]
    allFileMatches pattern = filter (match pattern . view val) . enumFile

data ElementPattern = ElementPattern String (CElement -> Bool)
data StatementPattern = StatementPattern String (CStatement -> Bool)
data ExprPattern = ExprPattern String (CExpression -> Bool)

instance Show ElementPattern where
    show (ElementPattern str _) = str

instance Matchable ElementPattern CElement where
    match (ElementPattern _ f) = f

instance Show StatementPattern where
    show (StatementPattern str _) = str

instance Matchable StatementPattern CStatement where
    match (StatementPattern _ f) = f

instance Show ExprPattern where
    show (ExprPattern str _) = str

instance Matchable ExprPattern CExpression where
    match (ExprPattern _ f) = f

