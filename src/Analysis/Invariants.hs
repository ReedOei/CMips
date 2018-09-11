{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Invariants where

import Control.Applicative
import Control.Lens ((^.), set)
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.SBV as SBV

import Flow

import Analysis.Resolution

import Compiler.Context
import CLanguage
import Types

-- sbvOp :: (Ord a, Num a, SymWord a, Ord b, Num b, SymWord b) => BinaryOp -> Symbolic (SBV a) -> Symbolic (SBV a) -> Symbolic (SBV b)
sbvArithOp Add = Just (+)
sbvArithOp Minus = Just (-)
sbvArithOp Div = Just sQuot

sbvBinaryOp CEQ = Just (.==)
sbvBinaryOp CLT = Just (.<)
sbvBinaryOp CGT = Just (SBV..>)
sbvBinaryOp CLTE = Just (.<=)
sbvBinaryOp CGTE = Just (.>=)
sbvBinaryOp CNE = Just (./=)

-- smtArithExpr :: (SDivisible (SBV a), SymWord a, MonadState (Map String (Symbolic (SBV a))) s) => CExpression -> s (Symbolic (SBV a))
smtArithExpr (VarRef name) = do
    varMap <- get
    case Map.lookup name varMap of
        Just v -> pure $ Just v
        Nothing -> do
            -- TODO: Make this handle other types
            let newVar = exists name
            modify $ Map.insert name newVar
            pure $ Just newVar

smtArithExpr (CBinaryOp op a b) = do
    aSym <- smtArithExpr a
    bSym <- smtArithExpr b

    case sbvArithOp op of
        Just f -> pure $ f <$> aSym <*> bSym
        Nothing -> pure Nothing

-- smtBoolExpr (CBinaryOp op a b) = do
--     aSmt <- smtExpr a
--     bSmt <- smtExpr b

--     undefined

    -- case sbvBinaryOp op of
    --     Just f ->
    --         case (,) <$> aSmt <*> bSmt of
    --             Just (Left aArith, Left bArith) -> pure $ f aArith bArith
    --             Just (Right aBool, Right bBool) -> pure $ f aBool bBool
                -- _ -> pure Nothing
        -- Nothing -> pure Nothing

smtExpr a =
    case smtArithExpr a of
        Just expr -> Left <$> expr
        -- Nothing -> Right <$> smtBoolExpr a

-- buildSMT context = do
--     (expr, _) <- runStateT (smtBoolExpr (context ^. val)) Map.empty
--     case expr of
--         Just e -> pure $ e .== 1

processInvariants :: Parentable a => Context a -> Context a
processInvariants context =
    set invariants (newInvariants ++ concatMap expandInvariant (context ^. invariants)) context
    where
        newInvariants = fromMaybe [] $ forInvariant <$> findParent forStmt context

        forStmt (StatementParent stmt@ForStatement{}) = Just stmt
        forStmt _ = Nothing

expandInvariant :: Invariant -> [Invariant]
expandInvariant (InvariantExpr (CBinaryOp And a b)) = concatMap expandInvariant [InvariantExpr a,InvariantExpr b]
expandInvariant i = [i]

forInvariant :: Context CStatement -> [Invariant]
forInvariant context =
    case context ^. val of
        ForStatement (VarDef (Var _ (NamedType t) varName) (Just expr)) _ step body ->
            -- TODO: Make this clearer/better
            if t `elem` ["int", "float", "double", "unsigned int", "long", "unsigned long", "long long", "long int", "long long int"] then
                case step of
                    ExprStatement (CPrefix PreIncrement (VarRef v))
                        | v == varName &&
                          not (modifiesReference varName context)
                          -> [InvariantExpr (CBinaryOp CGTE (VarRef v) expr)]
                    ExprStatement (CPostfix PostIncrement (VarRef v))
                        | v == varName &&
                          not (modifiesReference varName context)
                          -> [InvariantExpr (CBinaryOp CGTE (VarRef v) expr)]
                    _ -> []
            else
                []
        _ -> []

