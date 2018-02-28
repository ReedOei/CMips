module Compiler where

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
data Global = Global [String] (Map String String) -- Map function names to labels.
    deriving Show

newtype Local = Local (Map String String) -- Map of registers to variable names.
    deriving Show

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
getNextLabel (Global labels funcs) labelType = (Global (newLabel:labels) funcs, newLabel)
    where n = length $ filter (labelType `isPrefixOf`) labels
          newLabel = if n > 0 then
                        labelType ++ "_" ++ show n
                     else
                        labelType

funcLabel :: Global -> String -> (Global, String)
funcLabel global funcName = (Global labels (Map.insert funcName funcLabel funcs), funcLabel)
    where (Global labels funcs, funcLabel) = getNextLabel global funcName

getFuncLabel :: String -> Global -> String
getFuncLabel funcName (Global _ funcs) =
    fromMaybe (error ("Undefined reference to: " ++ funcName)) $ Map.lookup funcName funcs

purgeRegType :: String -> Local -> Local
purgeRegType rType (Local registers) = Local $ Map.filterWithKey (\k _ -> not (rType `isPrefixOf` k)) registers

purgeRegTypeEnv :: String -> Environment -> Environment
purgeRegTypeEnv rType (Environment file global local) = Environment file global $ purgeRegType rType local

purgeRegTypesEnv :: [String] -> Environment -> Environment
purgeRegTypesEnv rTypes env = foldr purgeRegTypeEnv env rTypes

emptyEnvironment :: Environment
emptyEnvironment = Environment (CFile "" []) (Global [] Map.empty) (Local Map.empty)

newEnvironment :: CFile -> Environment
newEnvironment file = Environment file (Global [] Map.empty) (Local Map.empty)

compile :: CFile -> MIPSFile
compile file@(CFile fname elements) = MIPSFile fname instructions
    where
        (finalEnv, instructions) = foldl go (newEnvironment file, []) elements
        go (env, prev) element = let (newEnv, newInstructions) = compileElement env element in
                                     (newEnv, prev ++ newInstructions)

saveStack :: [String] -> [MIPSInstruction]
saveStack registers = Inst OP_SUB "sp" "sp" (show (4 * length registers)) : saveInstr
    where saveInstr = map (\(i, r) -> Inst OP_SW r (show (i * 4)) "sp") $ zip [0..] registers

restoreStack :: [String] -> [MIPSInstruction]
restoreStack registers = restoreInstr ++ [Inst OP_ADD "sp" "sp" (show (4 * length registers))]
    where restoreInstr = map (\(i, r) -> Inst OP_LW r (show (i * 4)) "sp") $ zip [0..] registers

compileElement :: Environment -> CElement -> (Environment, [MIPSInstruction])
compileElement (Environment file global local) (FuncDef t funcName args isConst statements) =
    (Environment finalFile finalGlobal local,
     Label label : saveStack saveRegs ++ body ++ restoreStack saveRegs ++ [Inst OP_JR "ra" "" ""])
    where
        (newGlobal, label) = funcLabel global funcName
        (newLocal, argInstr) = generateArgs args local
        (Environment finalFile finalGlobal finalLocal, instr) = compileStatements (Environment file newGlobal newLocal) statements
        body = argInstr ++ instr
        saveRegs =
            case find isJAL body of
                -- We only need to save the return address if there's a jal in the body.
                Nothing -> getRegsOfType "s" finalLocal
                Just _ -> "ra" : getRegsOfType "s" finalLocal

compileElement (Environment file global local) _ = (Environment file global local, [])

compileStatements :: Environment -> [CStatement] -> (Environment, [MIPSInstruction])
compileStatements env = foldl go (purgeRegTypeEnv "t" env, [])
    where
        go (env, instructions) statement =
            let (newEnv, newInstr) = compileStatement env statement in
                (newEnv, instructions ++ newInstr)

compileCondition :: String -> Environment -> CExpression -> (Environment, [MIPSInstruction])
compileCondition falseLabel env (CBinaryOp And a b) = (bEnv, aInstr ++ bInstr)
    where (aEnv, aInstr) = compileCondition falseLabel env a
          (bEnv, bInstr) = compileCondition falseLabel aEnv b
compileCondition falseLabel env (CBinaryOp op a b) = (bEnv, aInstr ++ bInstr ++ [Inst opposite aReg bReg falseLabel])
    where opposite = getBranchOpNeg op
          (aEnv, aReg, aInstr) = compileExpressionTemp env a
          (bEnv, bReg, bInstr) = compileExpressionTemp aEnv b

----------------------------------
-- Compile statements
----------------------------------
compileStatement :: Environment -> CStatement -> (Environment, [MIPSInstruction])
compileStatement (Environment file global local) (VarDef (Var (Type isConst varKind typeName) varName) ini) =
    case ini of
        -- If there's no initializer, all we need to do is take note of the fact that this variable exists
        Nothing -> (Environment file global newLocal, [])
        Just (LitInt n) -> (Environment file global newLocal, [Inst OP_LI reg (show n) ""])
        Just initializer ->
            let (newEnv, source, instructions) = compileExpressionTemp (Environment file global newLocal) initializer in
                (purgeRegTypeEnv "t" newEnv, instructions ++ [Inst OP_MOVE reg source ""])

    where (newLocal, reg) = useNextRegister "s" varName local

compileStatement (Environment file global local) (IfStatement cond body) =
    (purgeRegTypeEnv "t" finalEnv,
     Label labelStart : instr ++ bodyInstr ++ [Label labelEnd])
    where (globalLabelStart, labelStart) = getNextLabel global "if"
          (newGlobal, labelEnd) = getNextLabel globalLabelStart "if_end"
          (newEnv, instr) = compileCondition labelEnd (Environment file newGlobal local) cond
          (finalEnv, bodyInstr) = compileStatements newEnv body

compileStatement (Environment file global local) (WhileStatement cond body) =
    (purgeRegTypeEnv "t" finalEnv,
     Label labelStart : instr ++ bodyInstr ++ [Inst OP_J labelStart "" "", Label labelEnd])
    where (globalLabelStart, labelStart) = getNextLabel global "while"
          (newGlobal, labelEnd) = getNextLabel globalLabelStart "while_end"
          (newEnv, instr) = compileCondition labelEnd (Environment file newGlobal local) cond
          (finalEnv, bodyInstr) = compileStatements newEnv body

compileStatement env (ForStatement ini cond step body) =
    (purgeRegTypeEnv "t" finalEnv, instrIni ++ instr)
    where (newEnv, instrIni) = compileStatement env ini
          (finalEnv, instr) = compileStatement newEnv (WhileStatement cond (body ++ [step]))

compileStatement env (Return expr) =
    (purgeRegTypeEnv "t" newEnv,
     instr ++ [Inst OP_MOVE "v0" source ""])
     where (newEnv, source, instr) = compileExpressionTemp env expr

compileStatement env@(Environment file global local) (Assign assignKind lhs rhs) =
    let assignOp = case assignKind of
                Nothing -> Inst OP_MOVE reg source ""
                Just op -> Inst (opFind op) reg reg source in
        (purgeRegTypeEnv "t" newEnv,
         accessInstr ++ instr ++ [assignOp])
    where (newEnv, source, instr) = compileExpressionTemp env rhs
          (reg, accessInstr) =
                case lhs of
                    Left varName -> (getRegister varName local, [])
                    Right expr@(CArrayAccess _ _) ->
                        let (_, dest, accessInstr) = compileExpressionTemp env expr in
                            (dest, init accessInstr ++ [Inst OP_SW source "0" dest])

compileStatement env (ExprStatement expr) =
    let (newEnv, _, instr) = compileExpressionTemp env expr in
        (purgeRegTypeEnv "t" newEnv, instr)

---------------------------------------------
-- Compile Expression (using temp registers)
---------------------------------------------
compileExpressionTemp :: Environment -> CExpression -> (Environment, String, [MIPSInstruction])
compileExpressionTemp (Environment file global local) (VarRef varName) = (Environment file global local, getRegister varName local, [])
compileExpressionTemp (Environment file global local) (LitInt i) =
    (Environment file global newLocal, reg, [Inst OP_LI reg (show i) ""])
    where (newLocal, reg) = useNextRegister "t" (show i) local

compileExpressionTemp env (CArrayAccess varName expr) =
    (Environment file global newLocal, dest,
     instr ++ [Inst OP_MUL dest source "4", -- Calculate offest TODO: Get sizeof
               Inst OP_ADD dest dest reg, -- Calculate address to load.
               Inst OP_LW dest "0" dest]) -- Load memory
    where (newEnv@(Environment file global local), source, instr) = compileExpressionTemp env expr
          (newLocal, dest) = useNextRegister "t" (varName ++ "_access") local
          reg = getRegister varName local

compileExpressionTemp env (CBinaryOp op a b) =
    (Environment file global newLocal, reg, aInstr ++ bInstr ++ [Inst (opFind op) reg aReg bReg])
    where (aEnv, aReg, aInstr) = compileExpressionTemp env a
          (Environment file global local, bReg, bInstr) = compileExpressionTemp aEnv b
          (newLocal, reg) = useNextRegister "t" "temp" local

compileExpressionTemp (Environment file global local) (CPrefix PreIncrement a) = (Environment file global local, a, [Inst OP_ADD reg reg "1"])
    where reg = getRegister a local

compileExpressionTemp (Environment file global local) (CPostfix PostIncrement a) = (Environment file global local, a, [Inst OP_ADD reg reg "1"])
    where reg = getRegister a local

compileExpressionTemp env (FuncCall funcName args) =
    (Environment file global local, "v0",
     instr ++ argLoading ++ [Inst OP_JAL funcLabel "" ""])
    where
        funcLabel = getFuncLabel funcName global
        (Environment file global local, regs, instr) = foldl go (env, [], []) args
        argLoading = map (\(i, r) -> Inst OP_MOVE ("a" ++ show i) r "") $ zip [0..] regs
        go (curEnv, curRegs, curInstr) expr =
            let (newEnv, reg, newInstr) = compileExpressionTemp curEnv expr in
                (newEnv, reg:curRegs, curInstr ++ newInstr)

compileExpressionTemp (Environment file global local) _ = (Environment file global local, "", [])

