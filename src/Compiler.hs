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
        (finalEnv, instructions) = foldl go (newEnvironment file, []) elements
        go (env, prev) element = let (newEnv, newInstructions) = compileElement env element in
                                     (newEnv, prev ++ [newInstructions])

saveStack :: [String] -> [MIPSInstruction]
saveStack registers = Inst OP_SUB "sp" "sp" (show (4 * length registers)) : saveInstr
    where saveInstr = map (\(i, r) -> Inst OP_SW r (show (i * 4)) "sp") $ zip [0..] registers

restoreStack :: [String] -> [MIPSInstruction]
restoreStack registers = restoreInstr ++ [Inst OP_ADD "sp" "sp" (show (4 * length registers))]
    where restoreInstr = map (\(i, r) -> Inst OP_LW r (show (i * 4)) "sp") $ zip [0..] registers

compileElement :: Environment -> CElement -> (Environment, [MIPSInstruction])
compileElement (Environment file global local) (FuncDef t funcName args statements) =
    (Environment finalFile finalGlobal local,
     Label label : saveStack saveRegs ++ [Empty] ++ body ++ [Empty, Label funcEnd] ++ restoreStack saveRegs ++ [Inst OP_JR "ra" "" ""])
    where
        (tempGlobal, (label, funcEnd)) = funcLabel global funcName
        (newLocal, argInstr) = generateArgs args local
        newGlobal = setCurFunc funcName tempGlobal
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
compileCondition falseLabel env st@(CBinaryOp And a b) = (bEnv, aInstr ++ bInstr)
    where (aEnv, aInstr) = compileCondition falseLabel env a
          (bEnv, bInstr) = compileCondition falseLabel aEnv b
compileCondition falseLabel (Environment file global local) st@(CBinaryOp Or a b) =
    (bEnv, aInstr ++ [Label oneFalseLabel] ++ bInstr)
    where
        (newGlobal, oneFalseLabel) = getNextLabel global "one_false"
        (aEnv, aInstr) = compileCondition oneFalseLabel (Environment file newGlobal local) a
        (bEnv, bInstr) = compileCondition falseLabel aEnv b
compileCondition falseLabel env expr@(CBinaryOp op a b)
    | op `elem` [CEQ, CNE, CLT, CGT, CLTE, CGTE] = (bEnv, aInstr ++ bInstr ++ [Inst opposite aReg bReg falseLabel])
    | otherwise = compileCondition falseLabel env (CBinaryOp CNE expr (LitInt 0))
    where opposite = getBranchOpNeg op
          (aEnv, aReg, aInstr) = compileExpressionTemp env a
          (bEnv, bReg, bInstr) = compileExpressionTemp aEnv b
compileCondition falseLabel env expr = compileCondition falseLabel env (CBinaryOp CNE expr (LitInt 0))

-- Returns the label to go to in order to skip this block.
handleIfStatement :: Environment -> CStatement -> (Environment, String, [MIPSInstruction])
handleIfStatement (Environment file global local) st@(ElseBlock statements) =
    (newEnv, labelEnd, Comment (readable st) : instr ++ [Label labelEnd])
    where (newGlobal, labelEnd) = getNextLabel global "else_end"
          (newEnv, instr) = compileStatements (Environment file newGlobal local) statements

handleIfStatement (Environment file global local) st@(IfStatement cond branches body) =
    (purgeRegTypeEnv "t" branchEnv, labelEnd,
     [Comment (readable st), Label labelStart] ++ instr ++ bodyInstr ++ branchInstr)
    where (globalLabelStart, labelStart) = getNextLabel global "if"
          (newGlobal, labelEnd) = getNextLabel globalLabelStart "if_end"
          (newEnv, instr) = compileCondition labelEnd (Environment file newGlobal local) cond
          (finalEnv, bodyInstr) = compileStatements newEnv body
          (branchEnv, branchInstr) =
            case branches of
                Nothing -> (finalEnv, [Label labelEnd])
                Just branch ->
                    let (env, branchEnd, instrs) = handleIfStatement finalEnv branch in
                        (env, [Inst OP_J branchEnd "" "", Label labelEnd] ++ instrs) -- Make sure to skip this branch.

----------------------------------
-- Compile statements
----------------------------------
compileStatement :: Environment -> CStatement -> (Environment, [MIPSInstruction])
compileStatement (Environment file global local) st@(VarDef (Var (Type varKind typeName) varName) ini) =
    case ini of
        -- If there's no initializer, all we need to do is take note of the fact that this variable exists
        Nothing -> (Environment file global newLocal, [Comment (readable st)])
        Just (LitInt n) -> (Environment file global newLocal, [Comment (readable st), Inst OP_LI reg (show n) ""])
        Just initializer ->
            let (newEnv, source, instructions) = compileExpressionTemp (Environment file global newLocal) initializer in
                (purgeRegTypeEnv "t" newEnv, Comment (readable st) : instructions ++ [Inst OP_MOVE reg source ""])

    where (newLocal, reg) = useNextRegister "s" varName local

compileStatement env ifStatement@IfStatement{} = (newEnv, instr)
    where (newEnv, _, instr) = handleIfStatement env ifStatement

compileStatement (Environment file global local) st@(WhileStatement cond body) =
    (purgeRegTypeEnv "t" finalEnv,
     [Comment (readable st), Label labelStart] ++ instr ++ bodyInstr ++ [Inst OP_J labelStart "" "", Label labelEnd])
    where (globalLabelStart, labelStart) = getNextLabel global "while"
          (newGlobal, labelEnd) = getNextLabel globalLabelStart "while_end"
          (newEnv, instr) = compileCondition labelEnd (Environment file newGlobal local) cond
          (finalEnv, bodyInstr) = compileStatements newEnv body

compileStatement env st@(ForStatement ini cond step body) =
    (purgeRegTypeEnv "t" finalEnv, Comment (readable st): instrIni ++ instr)
    where (newEnv, instrIni) = compileStatement env ini
          (finalEnv, instr) = compileStatement newEnv (WhileStatement cond (body ++ [step]))

compileStatement env st@(Return expr) =
    (purgeRegTypeEnv "t" newEnv,
     Comment (readable st) : instr ++ [Inst OP_MOVE "v0" source "", Inst OP_J funcEnd "" ""])
     where (newEnv@(Environment _ global _), source, instr) = compileExpressionTemp env expr
           (_, funcEnd) = getFuncLabel (getCurFunc global) global

compileStatement env@(Environment file global local) st@(Assign assignKind lhs rhs) =
    (purgeRegTypeEnv "t" newEnv,
     Comment (readable st) : accessInstr ++ instr ++ postAccessInstr)
    where
        assignOp = case assignKind of
                Nothing -> Inst OP_MOVE reg source ""
                Just op -> Inst (opFind op) reg reg source
        (newEnv, source, instr) = compileExpressionTemp env rhs
        (reg, accessInstr, postAccessInstr) =
                case lhs of
                    Left varName -> (getRegister varName local, [], [assignOp])
                    Right expr@(CArrayAccess _ _) ->
                        let (_, dest, accessInstr) = compileExpressionTemp env expr in
                            (dest, init accessInstr, [Inst OP_SW source "0" dest])
                    Right expr@(CPrefix Dereference (VarRef varName)) ->
                        let tempReg = getRegister varName local in
                            (tempReg, [], [Inst OP_SW source "0" tempReg])

compileStatement env st@(ExprStatement expr) =
    let (newEnv, _, instr) = compileExpressionTemp env expr in
        (purgeRegTypeEnv "t" newEnv, Comment (readable st) : instr)

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
     instr ++
               [Inst OP_MUL dest source $ show size,
                Inst OP_ADD dest dest reg, -- Calculate address to load.
                Inst OP_LW dest "0" dest]) -- Load memory
    where (newEnv@(Environment file global local), source, instr) = compileExpressionTemp env expr
          (newLocal, dest) = useNextRegister "t" (varName ++ "_access") local
          (Var varType _) = resolve varName file
          size = sizeof varType
          reg = getRegister varName local

compileExpressionTemp env (CBinaryOp op a b) =
    case op of
        -- In the case of CEQ, we compare for equality via:
        -- li reg, 1
        -- beq a, b, end
        -- li reg, 0 # Will be skipped if the two are equal
        -- end:
        CEQ -> let (newGlobal, endEqualityTest) = getNextLabel global "end_eq_test" in
                   (Environment file newGlobal newLocal, reg,
                    aInstr ++ bInstr ++ [Inst OP_LI reg "1" "", Inst OP_BNE aReg bReg endEqualityTest, Inst OP_LI reg "0" "", Label endEqualityTest])
        _ -> (Environment file global newLocal, reg, aInstr ++ bInstr ++ [Inst (opFind op) reg aReg bReg])
    where (aEnv, aReg, aInstr) = compileExpressionTemp env a
          (Environment file global local, bReg, bInstr) = compileExpressionTemp aEnv b
          (newLocal, reg) = useNextRegister "t" "temp" local

compileExpressionTemp env@(Environment file global local) (CPrefix PreIncrement a) =
    (Environment file global local, source, instr ++ [Inst OP_ADD source source "1"])
    where
        (newEnv, source, instr) = compileExpressionTemp env a

compileExpressionTemp env@(Environment file global local) (CPrefix PreDecrement a) =
    (Environment file global local, source, instr ++ [Inst OP_SUB source source "1"])
    where
        (newEnv, source, instr) = compileExpressionTemp env a

compileExpressionTemp env@(Environment file global local) (CPostfix PostIncrement a) =
    (Environment file global local, source, instr ++ [Inst OP_ADD source source "1"])
    where
        (newEnv, source, instr) = compileExpressionTemp env a

compileExpressionTemp env@(Environment file global local) (CPostfix PostDecrement a) =
    (Environment file global local, source, instr ++ [Inst OP_SUB source source "1"])
    where
        (newEnv, source, instr) = compileExpressionTemp env a

compileExpressionTemp env (FuncCall funcName args) =
    (Environment file global newLocal, reg,
     instr ++ argLoading ++
        [Inst OP_JAL funcLabel "" "",
         Inst OP_MOVE reg "v0" ""]) -- Make sure to save func call result.
    where
        (newLocal, reg) = useNextRegister "t" "func_call_return_val" local
        (funcLabel, _) = getFuncLabel funcName global
        (Environment file global local, regs, instr) = foldl go (env, [], []) args
        argLoading = map (\(i, r) -> Inst OP_MOVE ("a" ++ show i) r "") $ zip [0..] regs
        go (curEnv, curRegs, curInstr) expr =
            let (newEnv, reg, newInstr) = compileExpressionTemp curEnv expr in
                (newEnv, reg:curRegs, curInstr ++ newInstr)

compileExpressionTemp (Environment file global local) _ = (Environment file global local, "", [])

