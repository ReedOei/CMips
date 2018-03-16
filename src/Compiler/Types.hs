module Compiler.Types where

import Control.Monad.State

import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)

import CLanguage
import MIPSLanguage

import System.IO.Unsafe

data Environment = Environment CFile Data Global Local
    deriving Show

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
data Global = Global [String] (Map String (String, String)) String
    deriving Show

-- Contains:
-- Amount of stack space used.
-- Map of registers to variable names.
data Local = Local Int (Map String String) (Map String String)
    deriving Show

saveKData :: String -> String -> State Environment String
saveKData dataType dataVal = do
    Environment file (Data d kd) global local <- get

    let name = "kd" ++ show (length kd)
    let newD = DataElement name dataType dataVal : kd

    put $ Environment file (Data d newD) global local
    pure name

saveData :: String -> String -> State Environment String
saveData dataType dataVal = do
    Environment file (Data d kd) global local <- get

    let name = "d" ++ show (length d)
    let newD = DataElement name dataType dataVal : d

    put $ Environment file (Data newD kd) global local
    pure name

saveStr :: String -> State Environment String
saveStr = saveData "asciiz" . show

useNextRegister :: String -> String -> State Environment String
useNextRegister rtype str = do
    Environment file d global local@(Local stack registers stackLocs) <- get

    -- Note: This is guaranteed to succeed because we're searching through an infinite list.
    -- That being said, the register might not always be valid, but that's another problem.
    case find ((`Map.notMember` registers) . (rtype ++) . show) [0..] of
        Just regNum ->
            if (rtype == "s" && regNum <= 7) || (rtype == "t" && regNum <= 9) || rtype == "result_stack" then do
                if rtype == "result_stack" then do
                    offset <- stalloc 4
                    Environment _ _ _ (Local newStack _ _) <- get
                    put $ Environment file d global $ Local newStack (Map.insert (rtype ++ show regNum) str registers) (Map.insert (rtype ++ show regNum) (show offset) stackLocs)
                else
                    put $ Environment file d global $ Local stack (Map.insert (rtype ++ show regNum) str registers) stackLocs
                pure $ rtype ++ show regNum
            else
                -- We're out of registers :(. Time to start storing stuff on the stack.
                useNextRegister "result_stack" str

generateArgs :: [Var] -> State Environment [MIPSInstruction]
generateArgs [] = pure []
generateArgs (Var _ varName:args) = do
    reg <- useNextRegister "s" varName -- Use the s registers to store in case of function calls that would override.
    instr <- generateArgs args
    pure $ Inst OP_MOVE reg ("a" ++ tail reg) "" : instr

getRegsOfType :: String -> State Environment [String]
getRegsOfType rType = do
    Environment _ _ _ (Local _ registers _) <- get
    pure $ filter (rType `isPrefixOf`) $ Map.keys registers

registerNameExists :: String -> State Environment Bool
registerNameExists regName = do
    Environment _ _ _ (Local _ registers _) <- get
    pure $ isJust $ lookup regName $ map (\(a, b) -> (b, a)) $ Map.assocs registers

getRegister :: String -> State Environment String
getRegister varName = do
    Environment _ _ _ (Local _ registers _) <- get
    pure $ fromMaybe (error ("Undefined reference to: " ++ varName)) $
              lookup varName $ map (\(a, b) -> (b, a)) $ Map.assocs registers

getStackLoc :: String -> State Environment String
getStackLoc name = do
    Environment _ _ _ (Local _ _ stackLocs) <- get
    pure $ fromMaybe (error ("Undefined reference to: " ++ name)) $
            Map.lookup name stackLocs

getNextLabel :: String -> State Environment String
getNextLabel labelType = do
    Environment file d (Global labels funcs curFunc) local <- get

    let n = length $ filter (labelType `isPrefixOf`) labels
        newLabel = if n > 0 then
                        labelType ++ "_" ++ show n
                   else
                        labelType

    put $ Environment file d (Global (newLabel:labels) funcs curFunc) local
    pure newLabel

funcLabel :: String -> State Environment (String, String)
funcLabel funcName = do
    funcLabel <- getNextLabel funcName
    funcEnd <- getNextLabel $ funcName ++ "_end"
    Environment file d (Global labels funcs curFunc) local <- get
    put $ Environment file d (Global labels (Map.insert funcName (funcLabel, funcEnd) funcs) curFunc) local
    pure (funcLabel, funcEnd)

getFuncLabel :: String -> State Environment (Maybe (String, String))
getFuncLabel funcName = do
    Environment _ _ (Global _ funcs _) _ <- get
    pure $ Map.lookup funcName funcs

purgeRegType :: String -> Local -> Local
purgeRegType rType (Local stack registers stackLocs) = Local stack (Map.filterWithKey (\k _ -> not (rType `isPrefixOf` k)) registers) stackLocs

purgeRegTypeEnv :: String -> Environment -> Environment
purgeRegTypeEnv rType (Environment file d global local) = Environment file d global $ purgeRegType rType local

purgeRegTypesEnv :: [String] -> Environment -> Environment
purgeRegTypesEnv rTypes env = foldr purgeRegTypeEnv env rTypes

emptyEnvironment :: Environment
emptyEnvironment = Environment (CFile "" []) (Data [] []) (Global [] Map.empty "") (Local 0 Map.empty Map.empty)

newEnvironment :: CFile -> Environment
newEnvironment file = Environment file (Data [] []) (Global [] Map.empty "") (Local 0 Map.empty Map.empty)

resetStack :: Environment -> Environment
resetStack (Environment file d global (Local _ registers stackLocs)) = Environment file d global $ Local 0 registers stackLocs

setCurFunc :: String -> State Environment ()
setCurFunc curFunc = modify (\(Environment file d (Global labels funcs _) local) -> Environment file d (Global labels funcs curFunc) local)

getCurFunc :: State Environment String
getCurFunc = do
    Environment _ _ (Global _ _ curFunc) _ <- get
    pure curFunc

stalloc :: Int -> State Environment Int
stalloc amount = do
    Environment file d global (Local stack registers stackLocs) <- get
    put $ Environment file d global (Local (stack + amount) registers stackLocs)
    pure stack

