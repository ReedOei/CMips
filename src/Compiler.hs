module Compiler where

import Data.List (isPrefixOf, find)

import Data.Map (Map)
import qualified Data.Map as Map

import CLanguage
import MIPSLanguage

data Environment = Environment CFile Global Local
    deriving Show

newtype Global = Global [String] -- List of labels that have been used.
    deriving Show

newtype Local = Local (Map String String) -- Map of registers to variable names.
    deriving Show

useNextRegister :: String -> String -> Local -> (Local, String)
useNextRegister rtype str local@(Local registers) =
    case find (`Map.notMember` registers) $ map ((rtype ++) . show) [0..9] of
        Just register -> (Local (Map.insert register str registers), register)

generateArgs :: [Var] -> Local -> Local
generateArgs [] local = local
generateArgs (Var _ varName:args) local = generateArgs args newLocal
    where (newLocal, _) = useNextRegister "a" varName local

getRegister :: String -> Local -> Maybe String
getRegister varName (Local registers) = lookup varName $ map (\(a, b) -> (b, a)) $ Map.assocs registers

getNextLabel :: Global -> String -> (Global, String)
getNextLabel (Global labels) labelType = (Global (newLabel:labels), newLabel)
    where n = length $ filter (labelType `isPrefixOf`) labels
          newLabel = labelType ++ "_" ++ show n

purgeTemp :: Local -> Local
purgeTemp (Local registers) = Local $ Map.filterWithKey (\k _ -> not ("t" `isPrefixOf` k)) registers

purgeTempEnv :: Environment -> Environment
purgeTempEnv (Environment file global local) = Environment file global $ purgeTemp local

emptyEnvironment :: Environment
emptyEnvironment = Environment (CFile "" []) (Global []) (Local Map.empty)

newEnvironment :: CFile -> Environment
newEnvironment file = Environment file (Global []) (Local Map.empty)

compile :: CFile -> MIPSFile
compile file@(CFile fname elements) = MIPSFile fname instructions
    where
        (finalEnv, instructions) = foldl go (newEnvironment file, []) elements
        go (env, prev) element = let (newEnv, newInstructions) = compileElement env element in
                                     (newEnv, prev ++ newInstructions)

compileElement :: Environment -> CElement -> (Environment, [MIPSInstruction])
compileElement (Environment file global local) (FuncDef t funcName args isConst statements) =
    foldl go (Environment file newGlobal newLocal, []) statements
    where (newGlobal, label) = getNextLabel global funcName
          newLocal = generateArgs args local
          go (env, instructions) statement =
            let (newEnv, newInstr) = compileStatement env statement in
                (newEnv, instructions ++ newInstr)

compileElement (Environment file global local) _ = (Environment file global local, [])

compileStatement :: Environment -> CStatement -> (Environment, [MIPSInstruction])
compileStatement (Environment file global local) (VarDef (Var (Type isConst varKind typeName) varName) ini) =
    case ini of
        -- If there's no initializer, all we need to do is take note of the fact that this variable exists
        Nothing -> (Environment file global newLocal, [])
        Just initializer ->
            let (newEnv, source, instructions) = compileExpressionTemp (Environment file global newLocal) initializer in
                (purgeTempEnv newEnv, instructions ++ [Inst OP_MOVE reg source ""])

    where (newLocal, reg) = useNextRegister "s" varName local

compileStatement (Environment file global local) _ = (Environment file global local, [])

compileExpressionTemp :: Environment -> CExpression -> (Environment, String, [MIPSInstruction])
compileExpressionTemp (Environment file global local) (VarRef varName) =
    case getRegister varName local of
        Nothing -> error $ "Undefined reference to: " ++ varName
        Just reg -> (Environment file global local, reg, [])

compileExpressionTemp (Environment file global local) (LitInt i) =
    (Environment file global newLocal, reg, [Inst OP_LI reg (show i) ""])
    where (newLocal, reg) = useNextRegister "t" (show i) local

compileExpressionTemp env (CArrayAccess varName expr) =
    case getRegister varName local of
        Nothing -> error $ "Undefined reference to: " ++ varName
        Just reg ->
            (Environment file global newLocal, dest,
             instr ++ [Inst OP_MUL dest source "4", -- Calculate offest
                       Inst OP_ADD dest dest reg, -- Calculate address to load.
                       Inst OP_LW dest "0" dest]) -- Load memory

    where (newEnv@(Environment file global local), source, instr) = compileExpressionTemp env expr
          (newLocal, dest) = useNextRegister "t" (varName ++ "_access") local

compileExpressionTemp env (CBinaryOp Add a b) =
    (Environment file global newLocal, reg, aInstr ++ bInstr ++ [Inst OP_ADD reg aReg bReg])
    where (aEnv, aReg, aInstr) = compileExpressionTemp env a
          (Environment file global local, bReg, bInstr) = compileExpressionTemp aEnv b
          (newLocal, reg) = useNextRegister "t" "temp" local

compileExpressionTemp (Environment file global local) _ = (Environment file global local, "", [])

compileExpression :: Environment -> CExpression -> (Environment, [MIPSInstruction])
compileExpression (Environment file global local) _ = (Environment file global local, [])

