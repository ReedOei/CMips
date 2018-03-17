{-# LANGUAGE TupleSections #-}

module Compiler.Compiler where

import Control.Monad
import Control.Monad.State

import Data.Char (ord)
import Data.List (isPrefixOf, find, findIndex, intersperse, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)

import Compiler.Resolver
import Compiler.Types
import CLanguage
import MIPSLanguage
import Util

import System.IO.Unsafe

compile :: CFile -> MIPSFile
compile file@(CFile fname elements) = MIPSFile fname sections instructions
    where
        sections = [generateDataSection "data" d, generateDataSection "kdata" kd, MIPSSection "text" []]
        (instructions, Environment _ (Data d kd) _ _) = runState state $ newEnvironment file
        state = foldM go [] elements
        go prev element = do
            newInstructions <- compileElement element
            modify $ purgeRegTypesEnv ["s", "t", "result_stack"]
            modify resetStack

            return $ prev ++ [newInstructions]

generateDataSection :: String -> [DataElement] -> MIPSSection
generateDataSection sectionName elements =
    MIPSSection sectionName $ map (\(DataElement name dataType value) -> (name, dataType, value)) elements

saveStack :: Int -> [String] -> [MIPSInstruction]
saveStack reserved registers = Inst OP_SUB "sp" "sp" (show (reserved + 4 * length registers)) : saveInstr
    where saveInstr = map (\(i, r) -> Inst OP_SW r (show (reserved + i * 4)) "sp") $ zip [0..] registers

restoreStack :: Int -> [String] -> [MIPSInstruction]
restoreStack reserved registers = restoreInstr ++ [Inst OP_ADD "sp" "sp" (show (reserved + 4 * length registers))]
    where restoreInstr = map (\(i, r) -> Inst OP_LW r (show (reserved + i * 4)) "sp") $ zip [0..] registers

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

    finalInstr <- handleStackAccess body
    -- let finalInstr = body

    reserved <- stalloc 0 -- Requesting 0 more bytes will give us the top
    modify $ purgeRegTypeEnv "result_stack"
    pure $ Label label : saveStack reserved saveRegs ++ [Empty] ++ finalInstr ++ [Empty, Label funcEnd] ++ restoreStack reserved saveRegs ++ freeMemory ++ [Inst OP_JR "ra" "" ""]
    where
        freeMemory =
            case funcName of
                "main" -> [Inst OP_LI "v0" "10" "", Inst SYSCALL "" "" ""]
                _ -> []

compileElement _ = pure []

getUnusedRegisters :: [String] -> [String]
getUnusedRegisters regs = avail \\ regs
    where avail = map (("t" ++) . show) [0..9] ++ map (("s" ++) . show) [0..7]

handleStackAccess :: [MIPSInstruction] -> State Environment [MIPSInstruction]
handleStackAccess instr = do
    case findSplit (hasOperand ("result_stack" `isPrefixOf`)) instr of
        Nothing -> pure instr
        Just (beforeLoad, Inst op a b c, afterLoad) ->
            case getUnusedRegisters [a,b,c] of
                [] -> error "Fatal: Cannot figure out how to load registers from stack. All s and t registers are unavailable!"
                (reg:_) -> do
                    let stackReg = head $ filter ("result_stack" `isPrefixOf`) [a,b,c]

                    offset <- stalloc 4
                    loadOffset <- getStackLoc stackReg

                    let startInstr = [Inst OP_SW reg (show offset) "sp", Inst OP_LW reg loadOffset "sp"]
                    let endInstr = [Inst OP_SW reg loadOffset "sp", Inst OP_LW reg (show offset) "sp"]

                    let newInstr
                            | a == stackReg = Inst op reg b c
                            | b == stackReg = Inst op a reg c
                            | c == stackReg = Inst op a b reg

                    -- Keep going until there is no more stack access to handle.
                    handleStackAccess $ beforeLoad ++ startInstr ++ [newInstr] ++ endInstr ++ afterLoad

compileStatements :: [CStatement] -> State Environment [MIPSInstruction]
compileStatements statements = do
    instr <- foldM go [] statements
    modify $ purgeRegTypeEnv "t"

    pure instr
    where
        go instructions statement = do
            newInstr <- compileStatement statement
            pure $ instructions ++ newInstr

compileCondition :: String -> String -> CExpression -> State Environment [MIPSInstruction]
compileCondition trueLabel falseLabel st@(CBinaryOp And a b) =  do
    aInstr <- compileCondition "" falseLabel a
    bInstr <- compileCondition "" falseLabel b

    pure $ aInstr ++ bInstr ++ [Inst OP_J trueLabel "" ""]

compileCondition trueLabel falseLabel st@(CBinaryOp Or a b) = do
    aInstr <- compileCondition trueLabel "" a
    bInstr <- compileCondition trueLabel "" b

    pure $ aInstr ++ bInstr ++ [Inst OP_J falseLabel "" ""]

compileCondition trueLabel falseLabel expr@(CBinaryOp op a b) =
    let opposite = getBranchOpNeg op in do
        (aReg, aInstr) <- compileExpression a
        (bReg, bInstr) <- compileExpression b

        res <- if op `elem` [CEQ, CNE, CLT, CGT, CLTE, CGTE] then
                    if not $ null falseLabel then
                        pure $ aInstr ++ bInstr ++ [Inst opposite aReg bReg falseLabel]
                    else
                        pure $ aInstr ++ bInstr ++ [Inst (getBranchOp op) aReg bReg trueLabel]
                else
                    compileCondition trueLabel falseLabel (CBinaryOp CNE expr (LitInt 0))

        if countFunctionCalls expr > 0 then
            makeCallSafe res
        else
            pure res
compileCondition trueLabel falseLabel expr = compileCondition trueLabel falseLabel (CBinaryOp CNE expr (LitInt 0))

-- Returns the label to go to in order to skip this block.
handleIfStatement :: CStatement -> State Environment (String, [MIPSInstruction])
handleIfStatement st@(ElseBlock statements) = do
    labelEnd <- getNextLabel "else_end"
    instr <- compileStatements statements

    pure (labelEnd, Empty : Comment (readable st) : instr ++ [Label labelEnd])

handleIfStatement st@(IfStatement cond branches body) = do
    labelStart <- getNextLabel "if"
    labelBody <- getNextLabel "if_body"
    labelEnd <- getNextLabel "if_end"

    instr <- compileCondition labelBody labelEnd cond
    finalEnv <- get

    bodyInstr <- compileStatements body
    branchInstr <-
        case branches of
            Nothing -> pure [Label labelEnd]
            Just branch -> do
                (branchEnd, instrs) <- handleIfStatement branch
                pure $ [Inst OP_J branchEnd "" "", Label labelEnd] ++ instrs

    modify $ purgeRegTypeEnv "t"
    pure (labelEnd, [Empty, Comment (readable st), Label labelStart] ++ instr ++ [Label labelBody] ++ bodyInstr ++ branchInstr)

----------------------------------
-- Compile statements
----------------------------------
compileStatement :: CStatement -> State Environment [MIPSInstruction]
compileStatement st@(VarDef (Var FunctionPointer{} varName) ini) = do
    reg <- useNextRegister "s" varName


    case ini of
        Just (VarRef name) -> do
            fInfo <- getFuncLabel name
            case fInfo of
                Just (funcLabel, _) -> pure [Empty, Comment (readable st), Inst OP_LA reg funcLabel ""]
        Just initializer -> do
            (source, instructions) <- compileExpression initializer
            modify $ purgeRegTypeEnv "t"
            pure $ Empty : Comment (readable st) : instructions ++ [Inst OP_MOVE reg source ""]
        Nothing -> pure [Empty, Comment (readable st)]

compileStatement (VarDef (Var (NamedType name) varName) ini) = compileStatement (VarDef (Var (Type Value (NamedType name)) varName) ini)
compileStatement st@(VarDef (Var (Type varKind typeName) varName) ini) = do
    reg <- useNextRegister "s" varName

    case ini of
        -- If there's no initializer, all we need to do is take note of the fact that this variable exists
        Nothing -> pure [Empty, Comment (readable st)]
        Just (LitInt n) -> pure [Empty, Comment (readable st), Inst OP_LI reg (show n) ""]
        Just initializer -> do
            (source, instructions) <- compileExpression initializer
            modify $ purgeRegTypeEnv "t"
            pure $ Empty : Comment (readable st) : instructions ++ [Inst OP_MOVE reg source ""]


compileStatement ifStatement@IfStatement{} = do
    (_, instr) <- handleIfStatement ifStatement
    pure instr

compileStatement st@(WhileStatement cond body) = do
    labelStart <- getNextLabel "while"
    labelBody <- getNextLabel "while_body"
    labelEnd <- getNextLabel "while_end"

    instr <- compileCondition labelBody labelEnd cond
    bodyInstr <- compileStatements body

    modify $ purgeRegTypeEnv "t"
    pure $ [Empty, Comment (readable st), Label labelStart] ++ instr ++ [Label labelBody] ++ bodyInstr ++ [Inst OP_J labelStart "" "", Label labelEnd]

compileStatement st@(ForStatement ini cond step body) = do
    instrIni <- compileStatement ini
    instr <- compileStatement $ WhileStatement cond (body ++ [step])

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instrIni ++ instr

compileStatement st@(Return expr) = do
    (source, instr) <- compileExpression expr

    fInfo <- getFuncLabel =<< getCurFunc

    let funcEnd = case fInfo of
                    Nothing -> error "Unknown current function"
                    Just (_, endLabel) -> endLabel

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instr ++ [Inst OP_MOVE "v0" source "", Inst OP_J funcEnd "" ""]

compileStatement st@(Assign assignKind lhs rhs) = do
    (source, instr) <- compileExpression rhs

    (reg, accessInstr, postAccessInstr) <- do
            let assignOp x r = case assignKind of
                                Nothing -> [Inst OP_MOVE x source ""]
                                Just op -> [Inst (opFind op) x r source]

            (reg, instr) <- compileExpression lhs

            if not $ null instr then
                case last instr of
                    Inst OP_LW target offset loadSource -> do
                        tempReg <- useNextRegister "t" "temp_load"
                        pure (tempReg, init instr, assignOp tempReg target ++ [Inst OP_SW tempReg offset loadSource])
                    Inst OP_LB target offset loadSource -> do
                        tempReg <- useNextRegister "t" "temp_load"
                        pure (tempReg, init instr, assignOp tempReg target ++ [Inst OP_SB tempReg offset loadSource])
                    inst -> error $ "Unexpected instruction for assign expression: " ++ show st ++ " " ++ show inst
            else
                pure (reg, [], assignOp reg reg)

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : accessInstr ++ instr ++ postAccessInstr

compileStatement st@(ExprStatement expr) = do
    (_, instr) <- compileExpression expr

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instr

compileStatement CComment{} = pure []

---------------------------------------------
-- Compile Expressions
---------------------------------------------
countFunctionCalls :: CExpression -> Int
countFunctionCalls (FuncCall _ exprs) = 1 + sum (map countFunctionCalls exprs)
countFunctionCalls (MemberAccess a b) = countFunctionCalls a + countFunctionCalls b
countFunctionCalls (CPrefix _ expr) = countFunctionCalls expr
countFunctionCalls (CPostfix _ expr) = countFunctionCalls expr
countFunctionCalls (CArrayAccess a b) = countFunctionCalls a + countFunctionCalls b
countFunctionCalls (CBinaryOp _ a b) = countFunctionCalls a + countFunctionCalls b
countFunctionCalls _ = 0

getVarRefs :: CExpression -> [String]
getVarRefs (VarRef x) = [x]
getVarRefs (FuncCall fname exprs) = fname : concatMap getVarRefs exprs -- fname could be a var ref, if it is a function pointer variable.
getVarRefs (MemberAccess a b) = getVarRefs a -- Only refs in the front half will count, because the ones in the second half will just be members
getVarRefs (CPrefix _ expr) = getVarRefs expr
getVarRefs (CPostfix _ expr) = getVarRefs expr
getVarRefs (CArrayAccess a b) = getVarRefs a ++ getVarRefs b
getVarRefs (CBinaryOp _ a b) = getVarRefs a ++ getVarRefs b
getVarRefs _ = []

getReg :: String -> State Environment String
getReg r =
    if "t" `isPrefixOf` r then do
        exists <- registerNameExists r
        if exists then do
            rName <- getRegister r
            pure rName
        else
            useNextRegister "result_stack" r
    else
        pure r

useStack :: MIPSInstruction -> State Environment MIPSInstruction
useStack (Inst op a b c) = Inst op <$> getReg a <*> getReg b <*> getReg c
useStack i = pure i

makeCallSafe :: [MIPSInstruction] -> State Environment [MIPSInstruction]
makeCallSafe = mapM useStack

-- Compile expressions, but check if there is more than one function call in the expression.
-- If so, we will save all temp registers on the stack to be safe.
compileExpression :: CExpression -> State Environment (String, [MIPSInstruction])
compileExpression expr = do
    (reg, instr) <- compileExpressionTemp expr

    if countFunctionCalls expr > 0 then
        case expr of
            -- If there's only one function call and this expression is it, we don't need to waste time with saving stuff on the stack.
            FuncCall{} | countFunctionCalls expr == 1 -> pure (reg, instr)
            _ -> (,) <$> getReg reg <*> mapM useStack instr
    else
        pure (reg, instr)

compileExpressionTemp :: CExpression -> State Environment (String, [MIPSInstruction])
compileExpressionTemp (VarRef varName) = do
    fInfo <- getFuncLabel varName

    case fInfo of
        Just (funcLabel, _) -> do
            reg <- useNextRegister "t" $ varName ++ "_" ++ funcLabel

            pure (reg, [Inst OP_LA reg funcLabel ""])

        Nothing -> (,[]) <$> getRegister varName

compileExpressionTemp (LitInt i) = do
    reg <- useNextRegister "t" $ show i
    pure (reg, [Inst OP_LI reg (show i) ""])

compileExpressionTemp (LitChar c) = do
    reg <- useNextRegister "t" $ show $ ord c
    pure (reg, [Inst OP_LI reg (show (ord c)) ""])

compileExpressionTemp (LitString s) = do
    name <- saveStr s
    reg <- useNextRegister "t" name
    pure (reg, [Inst OP_LA reg name ""])

compileExpressionTemp NULL =  pure ("0", [])

compileExpressionTemp (CArrayAccess accessExpr expr) = do
    (source, instr) <- compileExpressionTemp expr
    (access, accessInstr) <- compileExpressionTemp accessExpr

    dest <- useNextRegister "t" $ readableExpr accessExpr ++ "_access"
    varType <- resolveType $ CPrefix Dereference accessExpr

    case sizeof varType of
        1 -> pure (dest, instr ++ accessInstr ++ [Inst OP_ADD dest source access, -- Calculate address to load.
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

-- t0 is just a dummy register because we should never use the value that comes from calling printf.
compileExpressionTemp (FuncCall "printf" args) = ("t0",) <$> compilePrintf args
compileExpressionTemp (FuncCall "malloc" [arg]) = compileMalloc arg
compileExpressionTemp (FuncCall "sizeof" [arg]) = compileSizeof arg
-- compileExpressionTemp (FuncCall "free" [arg]) = compileFree arg -- TODO: Figure this out.

compileExpressionTemp (FuncCall funcName args) = do
    let go (curRegs, curInstr) expr = do
            (reg, newInstr) <- compileExpressionTemp expr
            pure (curRegs ++ [reg], curInstr ++ newInstr)

    (regs, instr) <- foldM go ([], []) args

    let argLoading = map (\(i, r) -> Inst OP_MOVE ("a" ++ show i) r "") $ zip [0..] regs

    res <- getFuncLabel funcName
    retVal <- useNextRegister "t" "func_call_return_val"

    jumpOp <- case res of
                    -- If we don't find it, see if we have a function pointer for it.
                    Nothing -> do
                        r <- getRegister funcName
                        pure $ Inst OP_JALR r "" ""
                    Just (label, _) -> pure $ Inst OP_JAL label "" ""
    pure (retVal,
             instr ++ argLoading ++ [jumpOp] ++
             [Inst OP_MOVE retVal "v0" ""]) -- Make sure to save func call result.

compileSizeof :: CExpression -> State Environment (String, [MIPSInstruction])
compileSizeof expr = do
    t <- resolveType expr >>= elaborateType
    reg <- useNextRegister "t" $ "sizeof(" ++ show expr ++ ")"

    pure (reg, [Inst OP_LI reg (show (sizeof t)) ""])

compileMalloc :: CExpression -> State Environment (String, [MIPSInstruction])
compileMalloc expr = do
    (reg, instr) <- compileExpressionTemp expr

    pure ("v0", instr ++ [Inst OP_LI "v0" "9" "", Inst OP_MOVE "a0" reg "", Inst SYSCALL "" "" ""])

compilePrintf :: [CExpression] -> State Environment [MIPSInstruction]
compilePrintf (LitString formatStr:args) = do
    -- Split up the format string and find the things we need to insert (only strings and integers for now).
    let elements = concatMap (intersperse "%d" . splitOn "%d") $ intersperse "%s" $ splitOn "%s" formatStr

    (_, instr) <- foldM go (args, []) elements
    pure instr

    where
        go x "" = pure x

        go (a:as, curInstr) "%s" = do
            (reg, instr) <- compileExpressionTemp a
            t <- resolveType a >>= elaborateType
            case t of
                Type Pointer (NamedType "char") ->
                    pure (as, curInstr ++ instr ++ [Inst OP_MOVE "a0" reg "", Inst OP_LI "v0" "4" "", Inst SYSCALL "" "" ""]) -- 4 is print string
                _ -> error $ "Type is not char*, cannot print: '" ++ show t ++ "'"

        go (a:as, curInstr) "%d" = do
            (reg, instr) <- compileExpressionTemp a
            pure (as, curInstr ++ instr ++ [Inst OP_MOVE "a0" reg "", Inst OP_LI "v0" "1" "", Inst SYSCALL "" "" ""]) -- 1 is print int.

        go (as, curInstr) str = do
            name <- saveStr str
            reg <- useNextRegister "t" name
            pure (as, curInstr ++ [Inst OP_LA "a0" name "", Inst OP_LI "v0" "4" "", Inst SYSCALL "" "" ""]) -- 4 is print string

