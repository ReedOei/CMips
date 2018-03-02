module Compiler where

import Control.Monad
import Control.Monad.State

import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import CLanguage
import MIPSLanguage

import System.IO.Unsafe

data Environment = Environment CFile Global Local
    deriving Show

-- List of labels that have been used.
data Global = Global [String] (Map String (String, String)) String -- Map function names to labels.
    deriving Show

newtype Local = Local (Map String String) -- Map of registers to variable names.
    deriving Show

sizeof :: Type -> Int
sizeof (Type _ "char") = 1
sizeof (Type _ "unsigned char") = 1
sizeof (Type _ "unsigned int") = 4
sizeof (Type _ "int") = 4
sizeof _ = 4

findTypesElement :: CElement -> [Var]
findTypesElement (FuncDef _ _ vars statements) =
    vars ++ concatMap findTypesStatement statements

findTypesStatement :: CStatement -> [Var]
findTypesStatement (VarDef var _) = [var]
findTypesStatement (ForStatement ini _ _ block) = findTypesStatement ini ++ concatMap findTypesStatement block
findTypesStatement (IfStatement _ branch statements) = fromMaybe [] (findTypesStatement <$> branch) ++ concatMap findTypesStatement statements
findTypesStatement (ElseBlock statements) = concatMap findTypesStatement statements
findTypesStatement (WhileStatement _ statements) = concatMap findTypesStatement statements
findTypesStatement _ = []

resolve :: String -> CFile -> Var
resolve refName (CFile _ elements) =
    fromMaybe (error ("Unknown reference to: " ++ refName)) $
        find (\(Var _ varName) -> varName == refName) $ concatMap findTypesElement elements

useNextRegister :: String -> String -> Local -> (Local, String)
useNextRegister rtype str local@(Local registers) =
    case find (`Map.notMember` registers) $ map ((rtype ++) . show) [0..9] of
        Just register -> (Local (Map.insert register str registers), register)

generateArgs :: [Var] -> Local -> (Local, [MIPSInstruction])
generateArgs [] local = (local, [])
generateArgs (Var _ varName:args) local = (finalLocal, Inst OP_MOVE reg ("a" ++ tail reg) "" : instr)
    where (newLocal, reg) = useNextRegister "s" varName local -- Use the s registers to store in case of function calls that would override.
          (finalLocal, instr) = generateArgs args newLocal

getRegsOfType :: String -> Local -> [String]
getRegsOfType rType (Local registers) = filter (rType `isPrefixOf`) $ Map.keys registers

getRegister :: String -> Local -> String
getRegister varName (Local registers) =
    fromMaybe (error ("Undefined reference to: " ++ varName)) $
              lookup varName $ map (\(a, b) -> (b, a)) $ Map.assocs registers

getNextLabel :: Global -> String -> (Global, String)
getNextLabel (Global labels funcs curFunc) labelType = (Global (newLabel:labels) funcs curFunc, newLabel)
    where n = length $ filter (labelType `isPrefixOf`) labels
          newLabel = if n > 0 then
                        labelType ++ "_" ++ show n
                     else
                        labelType

funcLabel :: Global -> String -> (Global, (String, String))
funcLabel global funcName = (Global labels (Map.insert funcName (funcLabel, funcEnd) funcs) curFunc, (funcLabel, funcEnd))
    where (newGlobal, funcLabel) = getNextLabel global funcName
          (Global labels funcs curFunc, funcEnd) = getNextLabel global $ funcName ++ "_end"

getFuncLabel :: String -> Global -> (String, String)
getFuncLabel funcName (Global _ funcs _) =
    fromMaybe (error ("Undefined reference to: " ++ funcName)) $ Map.lookup funcName funcs

purgeRegType :: String -> Local -> Local
purgeRegType rType (Local registers) = Local $ Map.filterWithKey (\k _ -> not (rType `isPrefixOf` k)) registers

purgeRegTypeEnv :: String -> Environment -> Environment
purgeRegTypeEnv rType (Environment file global local) = Environment file global $ purgeRegType rType local

purgeRegTypesEnv :: [String] -> Environment -> Environment
purgeRegTypesEnv rTypes env = foldr purgeRegTypeEnv env rTypes

emptyEnvironment :: Environment
emptyEnvironment = Environment (CFile "" []) (Global [] Map.empty "") (Local Map.empty)

newEnvironment :: CFile -> Environment
newEnvironment file = Environment file (Global [] Map.empty "") (Local Map.empty)

setCurFunc :: String -> Global -> Global
setCurFunc curFunc (Global labels funcs _) = Global labels funcs curFunc

getCurFunc :: Global -> String
getCurFunc (Global _ _ curFunc) = curFunc

compile :: CFile -> MIPSFile
compile file@(CFile fname elements) = MIPSFile fname instructions
    where
        (instructions, finalEnv) = runState state $ newEnvironment file
        state = foldM go [] elements
        go prev element = do
            env <- get
            newInstructions <- compileElement element

            return $ prev ++ [newInstructions]

saveStack :: [String] -> [MIPSInstruction]
saveStack registers = Inst OP_SUB "sp" "sp" (show (4 * length registers)) : saveInstr
    where saveInstr = map (\(i, r) -> Inst OP_SW r (show (i * 4)) "sp") $ zip [0..] registers

restoreStack :: [String] -> [MIPSInstruction]
restoreStack registers = restoreInstr ++ [Inst OP_ADD "sp" "sp" (show (4 * length registers))]
    where restoreInstr = map (\(i, r) -> Inst OP_LW r (show (i * 4)) "sp") $ zip [0..] registers

compileElement :: CElement -> State Environment [MIPSInstruction]
compileElement (FuncDef t funcName args statements) = do
    Environment file global local <- get
    let (tempGlobal, (label, funcEnd)) = funcLabel global funcName
        (newLocal, argInstr) = generateArgs args local
        newGlobal = setCurFunc funcName tempGlobal in do

        put $ Environment file newGlobal newLocal
        instr <- compileStatements statements

        Environment finalFile finalGlobal finalLocal <- get

        let body = argInstr ++ instr
            saveRegs =
                case find isJAL body of
                    -- We only need to save the return address if there's a jal in the body.
                    Nothing -> getRegsOfType "s" finalLocal
                    Just _ -> "ra" : getRegsOfType "s" finalLocal in do

        put $ Environment finalFile finalGlobal local

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
    Environment file global local <- get
    let (newGlobal, oneFalseLabel) = getNextLabel global "one_false" in do
        put $ Environment file newGlobal local

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
    Environment file global local <- get
    let (newGlobal, labelEnd) = getNextLabel global "else_end" in do
        put $ Environment file newGlobal local

        instr <- compileStatements statements

        pure (labelEnd, Empty : Comment (readable st) : instr ++ [Label labelEnd])

handleIfStatement st@(IfStatement cond branches body) = do
    Environment file global local <- get
    let (globalLabelStart, labelStart) = getNextLabel global "if"
        (newGlobal, labelEnd) = getNextLabel globalLabelStart "if_end" in do

        put $ Environment file newGlobal local
        instr <- compileCondition labelEnd cond
        finalEnv <- get

        bodyInstr <- compileStatements body
        case branches of
            Nothing -> pure (labelEnd, [Label labelEnd])
            Just branch -> do
                (branchEnd, instrs) <- handleIfStatement branch

                modify $ purgeRegTypeEnv "t"

                pure (labelEnd, [Empty, Comment (readable st), Label labelStart] ++ instr ++ bodyInstr ++ [Inst OP_J branchEnd "" "", Label labelEnd] ++ instrs)

----------------------------------
-- Compile statements
----------------------------------
compileStatement :: CStatement -> State Environment [MIPSInstruction]
compileStatement st@(VarDef (Var (Type varKind typeName) varName) ini) = do
    Environment file global local <- get

    let (newLocal, reg) = useNextRegister "s" varName local in do
        put $ Environment file global newLocal
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
    Environment file global local <- get
    let (globalLabelStart, labelStart) = getNextLabel global "while"
        (newGlobal, labelEnd) = getNextLabel globalLabelStart "while_end" in do

        put $ Environment file newGlobal local
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

    let (_, funcEnd) = getFuncLabel (getCurFunc global) global in do
        modify $ purgeRegTypeEnv "t"
        pure $ Empty : Comment (readable st) : instr ++ [Inst OP_MOVE "v0" source "", Inst OP_J funcEnd "" ""]

compileStatement st@(Assign assignKind lhs rhs) = do
    (source, instr) <- compileExpressionTemp rhs

    env@(Environment file global local) <- get

    (reg, accessInstr, postAccessInstr) <- do
            let assignOp x r = case assignKind of
                                Nothing -> [Inst OP_MOVE x source ""]
                                Just op -> [Inst (opFind op) r r source]

            case lhs of
                Left varName -> pure (getRegister varName local, [], assignOp (getRegister varName local) (getRegister varName local))
                Right expr@(CArrayAccess _ _) -> do
                    (dest, accessInstr) <- compileExpressionTemp expr
                    Environment _ _ newLocal <- get

                    let (finalLocal, tempReg) = useNextRegister "t" "temp_array_load" newLocal

                    put $ Environment file global finalLocal
                    pure (dest, init accessInstr, [Inst OP_LW tempReg "0" dest] ++ assignOp dest tempReg ++ [Inst OP_SW tempReg "0" dest])
                Right expr@(CPrefix Dereference (VarRef varName)) ->
                    let tempReg = getRegister varName local in
                        pure (tempReg, [], assignOp tempReg source ++ [Inst OP_SW source "0" tempReg])

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : accessInstr ++ instr ++ postAccessInstr

compileStatement st@(ExprStatement expr) = do
    (_, instr) <- compileExpressionTemp expr

    modify $ purgeRegTypeEnv "t"
    pure $ Empty : Comment (readable st) : instr

---------------------------------------------
-- Compile Expression (using temp registers)
---------------------------------------------
compileExpressionTemp :: CExpression -> State Environment (String, [MIPSInstruction])
compileExpressionTemp (VarRef varName) = do
    Environment _ _ local <- get
    pure (getRegister varName local, [])

compileExpressionTemp (LitInt i) = do
    Environment file global local <- get
    let (newLocal, reg) = useNextRegister "t" (show i) local
    put $ Environment file global newLocal

    pure (reg, [Inst OP_LI reg (show i) ""])

compileExpressionTemp (CArrayAccess varName expr) = do
    env <- get
    (source, instr) <- compileExpressionTemp expr
    newEnv@(Environment file global local) <- get

    let (newLocal, dest) = useNextRegister "t" (varName ++ "_access") local
        (Var varType _) = resolve varName file
        size = sizeof varType
        reg = getRegister varName local in do

        put $ Environment file global newLocal
        pure (dest,
                 instr ++ [Inst OP_MUL dest source $ show size,
                           Inst OP_ADD dest dest reg, -- Calculate address to load.
                           Inst OP_LW dest "0" dest]) -- Load memory

compileExpressionTemp (CBinaryOp op a b) = do
    (aReg, aInstr) <- compileExpressionTemp a
    (bReg, bInstr) <- compileExpressionTemp b
    Environment file global local <- get
    let (newLocal, reg) = useNextRegister "t" "temp" local in
        case op of
            -- In the case of CEQ, we compare for equality via:
            -- li reg, 1
            -- beq a, b, end
            -- li reg, 0 # Will be skipped if the two are equal
            -- end:
            CEQ -> let (newGlobal, endEqualityTest) = getNextLabel global "end_eq_test" in do
                put $ Environment file newGlobal newLocal
                pure (reg, aInstr ++ bInstr ++ [Inst OP_LI reg "1" "", Inst OP_BNE aReg bReg endEqualityTest, Inst OP_LI reg "0" "", Label endEqualityTest])
            _ -> do
                put $ Environment file global newLocal
                pure (reg, aInstr ++ bInstr ++ [Inst (opFind op) reg aReg bReg])

compileExpressionTemp (CPrefix PreIncrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_ADD source source "1"])

compileExpressionTemp (CPrefix PreDecrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_SUB source source "1"])

compileExpressionTemp (CPrefix Dereference a) = do
    Environment file global local <- get
    let (newLocal, reg) = useNextRegister "t" "temp" local in do
        put $ Environment file global newLocal

        (source, instr) <- compileExpressionTemp a
        pure (source, instr ++ [Inst OP_LW reg source ""])

compileExpressionTemp (CPrefix PreNot a) = do
    (source, instr) <- compileExpressionTemp a
    Environment file global local <- get

    let (newGlobal, endNot) = getNextLabel global "end_not"
        (newLocal, reg) = useNextRegister "t" "temp" local in do

        put $ Environment file newGlobal newLocal
        pure (reg, instr ++ [Inst OP_LI reg "1" "", Inst OP_BEQ source "0" endNot, Inst OP_LI reg "0" "", Label endNot])

compileExpressionTemp (CPostfix PostIncrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_ADD source source "1"])

compileExpressionTemp (CPostfix PostDecrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_SUB source source "1"])

compileExpressionTemp (FuncCall funcName args) = do
    env <- get
    let go (curRegs, curInstr) expr = do
            (reg, newInstr) <- compileExpressionTemp expr
            pure (curRegs ++ [reg], curInstr ++ newInstr) in do

        (regs, instr) <- foldM go ([], []) args

        Environment file global local <- get

        let (newLocal, reg) = useNextRegister "t" "func_call_return_val" local
            (funcLabel, _) = getFuncLabel funcName global
            argLoading = map (\(i, r) -> Inst OP_MOVE ("a" ++ show i) r "") $ zip [0..] regs

        put $ Environment file global newLocal
        pure (reg,
                 instr ++ argLoading ++
                    [Inst OP_JAL funcLabel "" "",
                     Inst OP_MOVE reg "v0" ""]) -- Make sure to save func call result.

