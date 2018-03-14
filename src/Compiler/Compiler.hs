{-# LANGUAGE TupleSections #-}

module Compiler.Compiler where

import Control.Monad
import Control.Monad.State

import Data.Char (ord)
import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Compiler.Resolver
import Compiler.Types
import CLanguage
import MIPSLanguage

import System.IO.Unsafe

compile :: CFile -> MIPSFile
compile file@(CFile fname elements) = MIPSFile fname instructions
    where
        (instructions, finalEnv) = runState state $ newEnvironment file
        state = foldM go [] elements
        go prev element = do
            newInstructions <- compileElement element

            modify $ purgeRegTypesEnv ["s", "t"]

            return $ prev ++ [newInstructions]

saveStack :: [String] -> [MIPSInstruction]
saveStack registers = Inst OP_SUB "sp" "sp" (show (4 * length registers)) : saveInstr
    where saveInstr = map (\(i, r) -> Inst OP_SW r (show (i * 4)) "sp") $ zip [0..] registers

restoreStack :: [String] -> [MIPSInstruction]
restoreStack registers = restoreInstr ++ [Inst OP_ADD "sp" "sp" (show (4 * length registers))]
    where restoreInstr = map (\(i, r) -> Inst OP_LW r (show (i * 4)) "sp") $ zip [0..] registers

compileElement :: CElement -> State Environment [MIPSInstruction]
compileElement (FuncDef t funcName args statements) = do
    (label, funcEnd) <- funcLabel funcName

    setCurFunc funcName

    argInstr <- generateArgs args

    instr <- compileStatements statements

    let body = argInstr ++ instr
    saveRegs <-
        case find isJAL body of
            -- We only need to save the return address if there's a jal in the body.
            Nothing -> getRegsOfType "s"
            Just _ -> ("ra" :) <$> getRegsOfType "s"

    pure $ Label label : saveStack saveRegs ++ [Empty] ++ body ++ [Empty, Label funcEnd] ++ restoreStack saveRegs ++ [Inst OP_JR "ra" "" ""]

compileElement _ = pure []

compileStatements :: [CStatement] -> State Environment [MIPSInstruction]
compileStatements statements = do
    instr <- foldM go [] statements
    modify $ purgeRegTypeEnv "t"

    pure instr
    where
        go instructions statement = do
            env <- get
            newInstr <- compileStatement statement
            pure $ instructions ++ newInstr

compileCondition :: String -> CExpression -> State Environment [MIPSInstruction]
compileCondition falseLabel st@(CBinaryOp And a b) =  do
    aInstr <- compileCondition falseLabel a
    bInstr <- compileCondition falseLabel b

    pure $ aInstr ++ bInstr

compileCondition falseLabel st@(CBinaryOp Or a b) = do
    oneFalseLabel <- getNextLabel "one_false"

    aInstr <- compileCondition oneFalseLabel a
    bInstr <- compileCondition falseLabel b
    pure $ aInstr ++ [Label oneFalseLabel] ++ bInstr

compileCondition falseLabel expr@(CBinaryOp op a b) = do
    env <- get
    let opposite = getBranchOpNeg op in do
        (aReg, aInstr) <- compileExpressionTemp a
        (bReg, bInstr) <- compileExpressionTemp b

        if op `elem` [CEQ, CNE, CLT, CGT, CLTE, CGTE] then
            pure $ aInstr ++ bInstr ++ [Inst opposite aReg bReg falseLabel]
        else
            compileCondition falseLabel (CBinaryOp CNE expr (LitInt 0))
compileCondition falseLabel expr = compileCondition falseLabel (CBinaryOp CNE expr (LitInt 0))

-- Returns the label to go to in order to skip this block.
handleIfStatement :: CStatement -> State Environment (String, [MIPSInstruction])
handleIfStatement st@(ElseBlock statements) = do
    labelEnd <- getNextLabel "else_end"
    instr <- compileStatements statements

    pure (labelEnd, Empty : Comment (readable st) : instr ++ [Label labelEnd])

handleIfStatement st@(IfStatement cond branches body) = do
    labelStart <- getNextLabel "if"
    labelEnd <- getNextLabel "if_end"

    instr <- compileCondition labelEnd cond
    finalEnv <- get

    bodyInstr <- compileStatements body
    branchInstr <-
        case branches of
            Nothing -> pure [Label labelEnd]
            Just branch -> do
                (branchEnd, instrs) <- handleIfStatement branch
                pure $ [Inst OP_J branchEnd "" "", Label labelEnd] ++ instrs

    modify $ purgeRegTypeEnv "t"
    pure (labelEnd, [Empty, Comment (readable st), Label labelStart] ++ instr ++ bodyInstr ++ branchInstr)

----------------------------------
-- Compile statements
----------------------------------
compileStatement :: CStatement -> State Environment [MIPSInstruction]
compileStatement st@(VarDef (Var (Type varKind typeName) varName) ini) = do
    reg <- useNextRegister "s" varName

    case ini of
        -- If there's no initializer, all we need to do is take note of the fact that this variable exists
        Nothing -> pure [Empty, Comment (readable st)]
        Just (LitInt n) -> pure [Empty, Comment (readable st), Inst OP_LI reg (show n) ""]
        Just initializer -> do
            (source, instructions) <- compileExpressionTemp initializer
            modify $ purgeRegTypeEnv "t"
            pure $ Empty : Comment (readable st) : instructions ++ [Inst OP_MOVE reg source ""]


compileStatement ifStatement@IfStatement{} = do
    (_, instr) <- handleIfStatement ifStatement
    pure instr

compileStatement st@(WhileStatement cond body) = do
    labelStart <- getNextLabel "while"
    labelEnd <- getNextLabel "while_end"

    instr <- compileCondition labelEnd cond
    bodyInstr <- compileStatements body

    modify $ purgeRegTypeEnv "t"
    pure $ [Empty, Comment (readable st), Label labelStart] ++ instr ++ bodyInstr ++ [Inst OP_J labelStart "" "", Label labelEnd]

compileStatement st@(ForStatement ini cond step body) = do
    env <- get
    instrIni <- compileStatement ini
    instr <- compileStatement $ WhileStatement cond (body ++ [step])

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instrIni ++ instr

compileStatement st@(Return expr) = do
    env <- get

    (source, instr) <- compileExpressionTemp expr

    Environment _ global _ <- get

    (_, funcEnd) <- getFuncLabel =<< getCurFunc

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instr ++ [Inst OP_MOVE "v0" source "", Inst OP_J funcEnd "" ""]

compileStatement st@(Assign assignKind lhs rhs) = do
    (source, instr) <- compileExpressionTemp rhs

    Environment file global local <- get

    (reg, accessInstr, postAccessInstr) <- do
            let assignOp x r = case assignKind of
                                Nothing -> [Inst OP_MOVE x source ""]
                                Just op -> [Inst (opFind op) x r source]

            (reg, instr) <- compileExpressionTemp lhs

            if not $ null instr then
                case last instr of
                    Inst OP_LW target offset loadSource -> do
                        tempReg <- useNextRegister "t" "temp_load"
                        pure (tempReg, init instr, assignOp tempReg target ++ [Inst OP_SW tempReg offset target])
                    Inst OP_LB target offset loadSource -> do
                        tempReg <- useNextRegister "t" "temp_load"
                        pure (tempReg, init instr, assignOp tempReg target ++ [Inst OP_SB tempReg offset target])
                    inst -> error $ "Unexpected instruction for assign expression: " ++ show st ++ " " ++ show inst
            else
                pure (reg, [], assignOp reg reg)

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : accessInstr ++ instr ++ postAccessInstr

compileStatement st@(ExprStatement expr) = do
    (_, instr) <- compileExpressionTemp expr

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instr

compileStatement CComment{} = pure []

---------------------------------------------
-- Compile Expression (using temp registers)
---------------------------------------------
compileExpressionTemp :: CExpression -> State Environment (String, [MIPSInstruction])
compileExpressionTemp (VarRef varName) = (,[]) <$> getRegister varName

compileExpressionTemp (LitInt i) = do
    reg <- useNextRegister "t" $ show i
    pure (reg, [Inst OP_LI reg (show i) ""])

compileExpressionTemp (LitChar c) = do
    reg <- useNextRegister "t" $ show $ ord c
    pure (reg, [Inst OP_LI reg (show (ord c)) ""])

compileExpressionTemp NULL =  pure ("0", [])

compileExpressionTemp (CArrayAccess accessExpr expr) = do
    (source, instr) <- compileExpressionTemp expr
    (access, accessInstr) <- compileExpressionTemp accessExpr

    dest <- useNextRegister "t" $ readableExpr accessExpr ++ "_access"
    varType <- resolveType $ CPrefix Dereference accessExpr

    case sizeof varType of
        1 -> pure (dest, instr ++ accessInstr ++ [Inst OP_ADD dest dest access, -- Calculate address to load.
                                                  Inst OP_LB dest "0" dest]) -- Load memory, using lb instead of lw.
        size -> pure (dest, instr ++ accessInstr ++ [Inst OP_MUL dest source $ show $ sizeof varType,
                                                     Inst OP_ADD dest dest access, -- Calculate address to load.
                                                     Inst OP_LW dest "0" dest]) -- Load memory

compileExpressionTemp (CBinaryOp op a b) = do
    (aReg, aInstr) <-
        case b of
            FuncCall{} -> do -- If it's a func call, make sure we save the aReg to a shared reg instead of a temp.
                (tempReg, instr) <- compileExpressionTemp a
                savedReg <- useNextRegister "s" "temp_for_func_call"

                pure (savedReg, instr ++ [Inst OP_MOVE savedReg tempReg ""])
            _ -> compileExpressionTemp a

    (bReg, bInstr) <- compileExpressionTemp b
    reg <- useNextRegister "t" "temp"

    case op of
        -- In the case of CEQ, we compare for equality via:
        -- li reg, 1
        -- beq a, b, end
        -- li reg, 0 # Will be skipped if the two are equal
        -- end:
        CEQ -> do
            endEqualityTest <- getNextLabel "end_eq_test"
            pure (reg, aInstr ++ bInstr ++ [Inst OP_LI reg "1" "", Inst OP_BEQ aReg bReg endEqualityTest, Inst OP_LI reg "0" "", Label endEqualityTest])
        _ -> pure (reg, aInstr ++ bInstr ++ [Inst (opFind op) reg aReg bReg])

compileExpressionTemp (CPrefix PreIncrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_ADD source source "1"])

compileExpressionTemp (CPrefix PreDecrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_SUB source source "1"])

compileExpressionTemp (CPrefix Dereference a) = do
    reg <- useNextRegister "t" "temp"
    (source, instr) <- compileExpressionTemp a
    pure (reg, instr ++ [Inst OP_LW reg "0" source])

compileExpressionTemp (CPrefix PreNot a) = do
    (source, instr) <- compileExpressionTemp a
    endNot <- getNextLabel "end_not"
    reg <- useNextRegister "t" "temp"

    pure (reg, instr ++ [Inst OP_LI reg "1" "", Inst OP_BEQ source "0" endNot, Inst OP_LI reg "0" "", Label endNot])

compileExpressionTemp (CPostfix PostIncrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_ADD source source "1"])

compileExpressionTemp (CPostfix PostDecrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_SUB source source "1"])

compileExpressionTemp (MemberAccess expr (VarRef name)) = do
    (source, instr) <- compileExpressionTemp expr
    n <- getStructOffset expr name

    case last instr of
        Inst OP_LW a _ b -> pure (source, init instr ++ [Inst OP_LW a (show n) b])
        _ -> pure (source, instr ++ [Inst OP_LW source (show n) source])

compileExpressionTemp (FuncCall funcName args) = do
    env <- get
    let go (curRegs, curInstr) expr = do
            (reg, newInstr) <- compileExpressionTemp expr
            pure (curRegs ++ [reg], curInstr ++ newInstr)

    (regs, instr) <- foldM go ([], []) args

    let argLoading = map (\(i, r) -> Inst OP_MOVE ("a" ++ show i) r "") $ zip [0..] regs

    (funcLabel, _) <- getFuncLabel funcName
    reg <- useNextRegister "t" "func_call_return_val"

    pure (reg,
             instr ++ argLoading ++
                [Inst OP_JAL funcLabel "" "",
                 Inst OP_MOVE reg "v0" ""]) -- Make sure to save func call result.

