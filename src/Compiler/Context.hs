{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Compiler.Context where

import Control.Applicative
import Control.Lens (set, over, makeLenses, (^.))

import Data.Maybe

import Flow

import CLanguage
import Types
import Util

instance Parentable CFile where
    makeParent = FileParent

instance Parentable CElement where
    makeParent = ElementParent

instance Parentable CStatement where
    makeParent = StatementParent

instance Parentable CExpression where
    makeParent = ExprParent

elemContext :: Context CElement -> ([Invariant], b) -> Context b
elemContext context b =
    case context ^. val of
        FuncDef _ fname _ _ -> set funcName (Just fname) $ makeContext context b
        _ -> makeContext context b

makeContext :: Parentable a => Context a -> ([Invariant], b) -> Context b
makeContext context (invs, b) =
    over invariants (invs ++) $
    over parents (makeParent (context ^. val) :) $
    set val b context

enum :: (Enumerable t, Parentable a) => Context a -> [Context t]
enum context =
    case makeParent $ context ^. val of
        FileParent file -> enumFile file
        ElementParent element -> enumElement $ makeContext context $ ([],) element
        StatementParent statement -> enumStatement $ makeContext context $ ([],) statement
        ExprParent expr -> enumExpr $ makeContext context $ ([],) expr

instance Enumerable Var where
    enumElement context =
        let contexts = map (elemContext context) $
                case context ^. val of
                    FuncDef _ _ _ ss -> map ([],) ss
                    _ -> []
            vars = map (elemContext context) $
                case context ^. val of
                    FuncDef _ _ args _ -> map ([],) args
                    Inline  _ _ args _ -> map ([],) args
                    StructDef _ vars -> map ([],) vars
        in vars ++ concatMap enumStatement contexts

    enumStatement context =
        let statementContexts = map (makeContext context) $
                case context ^. val of
                    IfStatement cond elseBlock ss ->
                        map ([InvariantExpr cond],) $ ss ++ maybe [] (\(ElseBlock inner) -> inner) elseBlock
                    WhileStatement cond ss -> map ([InvariantExpr cond],) ss
                    ForStatement init cond step ss -> map ([InvariantExpr cond],) $ init : step : ss
                    ElseBlock ss -> map ([],) ss
                    Annotated _ (Just stmt) -> [([], stmt)]
                    _ -> []
            vars = map (makeContext context) $
                case context ^. val of
                    VarDef var (Just expr) -> [([], var)]
                    _ -> []
            in vars ++ concatMap enumStatement statementContexts

    enumExpr _ = []

instance Enumerable CElement where
    enumElement context = [context]
    enumStatement _ = []
    enumExpr _ = []

instance Enumerable CStatement where
    enumElement context =
        let contexts = map (elemContext context) $
                case context ^. val of
                    FuncDef _ _ _ ss -> map ([],) ss
                    _ -> []
        in contexts ++ concatMap enumStatement contexts

    enumStatement context =
        let contexts = map (makeContext context) $
                case context ^. val of
                    IfStatement cond elseBlock ss ->
                        map ([InvariantExpr cond],) $ ss ++ maybe [] (\(ElseBlock inner) -> inner) elseBlock
                    ElseBlock ss -> map ([],) ss
                    WhileStatement cond ss -> map ([InvariantExpr cond],) ss
                    fSt@(ForStatement init cond step ss) -> map ([InvariantExpr cond],) $ init : step : ss
                    _ -> []
            in contexts ++ concatMap enumStatement contexts

    enumExpr _ = []

instance Enumerable CExpression where
    -- Gather all the statements in the element, then enumerate through the expressions using the below
    enumElement context =
        let contexts = map (elemContext context) $
                case context ^. val of
                    FuncDef _ _ _ ss -> map ([],) ss
                    _ -> []
        in concatMap enumStatement contexts

    enumStatement context =
        let statementContexts = map (makeContext context) $
                case context ^. val of
                    IfStatement cond elseBlock ss ->
                        map ([InvariantExpr cond],) $ ss ++ maybe [] (\(ElseBlock inner) -> inner) elseBlock
                    WhileStatement cond ss -> map ([InvariantExpr cond],) ss
                    fSt@(ForStatement init cond step ss) -> map ([InvariantExpr cond],) $ init : step : ss
                    ElseBlock ss -> map ([],) ss
                    Annotated _ (Just stmt) -> [([], stmt)]
                    _ -> []
            exprContexts = map (makeContext context) $
                case context ^. val of
                    Return (Just expr) -> [([], expr)]
                    IfStatement cond _ _ -> [([], cond)]
                    WhileStatement cond _ -> [([], cond)]
                    fSt@(ForStatement _ cond _ _) -> [([], cond)]
                    Assign _ a b -> map ([],) [a,b]
                    ExprStatement expr -> [([], expr)]
                    VarDef _ (Just expr) -> [([], expr)]
                    _ -> []
            in exprContexts ++ concatMap enumStatement statementContexts ++ concatMap enumExpr exprContexts

    enumExpr context =
        let contexts = map (makeContext context) $
                case context ^. val of
                    MemberAccess a b -> map ([],) [a,b]
                    FuncCall _ exprs -> map ([],) exprs
                    CPrefix _ expr -> [([], expr)]
                    CPostfix _ expr -> [([], expr)]
                    CArrayAccess a b -> map ([],) [a, b]
                    CBinaryOp _ a b -> map ([],) [a, b]
                    _ -> []
        in contexts ++ concatMap enumExpr contexts

