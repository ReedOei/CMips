{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Control.Lens (makeLenses, (^.), set, over)

import Data.Map (Map)
import qualified Data.Map as Map

import CLanguage
import MIPSLanguage
import Util

data Invariant = InvariantExpr CExpression -- Either an expression that is true
               | InvariantAnnotation Annotation String -- Or an annotation for a particular variable
    deriving (Show, Eq)

data Parent = FileParent CFile
            | ElementParent CElement
            | StatementParent CStatement
            | ExprParent CExpression
    deriving (Show, Eq)

class Parentable a where
    makeParent :: a -> Parent

data Warning where
    Warning :: forall t. PrettyPrint t => Context t -> String -> Warning

data Context t = Context
    { _parents :: [Parent]
    , _funcName :: Maybe String
    , _invariants :: [Invariant]
    , _val :: t }
makeLenses ''Context

instance Show Warning where
    show (Warning context message) =
        case context ^. funcName of
            Nothing -> prettyPrint (context ^. val) ++ ": " ++ message
            Just fname -> fname ++ ": " ++ prettyPrint (context ^. val) ++ ": " ++ message

deriving instance Show t => Show (Context t)
deriving instance Eq t => Eq (Context t)

emptyContext = Context [] Nothing []

data CompileOptions = CompileOptions
    { _optimizeLevel :: Int
    , _useInlining :: Bool }
    deriving Show
makeLenses ''CompileOptions

defaultCompileOptions = CompileOptions 1 True

-- Name, type, value
-- e.g.:
--  str: .asciiz "hello world"
--  temp: .space 8
data DataElement = DataElement String String String
    deriving Show

-- Things to store in the data/kdata sections
data Data = Data [DataElement] [DataElement]
    deriving Show

-- Contains:
-- Map function names to labels.
-- List of labels that have been used.
data Global = Global
    { _labels :: [String]
    , _funcs :: Map String (String, String)
    , _curFunc :: String }
    deriving Show
makeLenses ''Global

-- Contains:
-- Amount of stack space used.
-- Map of registers to variable names.
data Local = Local
    { _stack :: Int
    , _registers :: Map String String
    , _stackLocs :: Map String String }
    deriving Show
makeLenses ''Local

data Environment = Environment
    { _file :: CFile
    , _dataSections :: Data
    , _global :: Global
    , _local :: Local
    , _compiled :: Map String [MIPSInstruction] -- The compiled assembly for all functions that have been compiled so far.
    , _warnings :: [Warning]
    , _compileOptions :: CompileOptions }
    deriving Show
makeLenses ''Environment

defaultContext file = Context [FileParent file] Nothing []

class Enumerable t where
    enumElement :: Context CElement -> [Context t]
    enumStatement :: Context CStatement -> [Context t]
    enumExpr :: Context CExpression -> [Context t]

    enumFile :: CFile -> [Context t]
    enumFile file@(CFile _ elements) = concatMap (enumElement . defaultContext file) elements

