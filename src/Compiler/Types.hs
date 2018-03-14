module Compiler.Types where

import Control.Monad.State

import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import CLanguage
import MIPSLanguage

data Environment = Environment CFile Global Local
    deriving Show

-- List of labels that have been used.
data Global = Global [String] (Map String (String, String)) String -- Map function names to labels.
    deriving Show

newtype Local = Local (Map String String) -- Map of registers to variable names.
    deriving Show

useNextRegister :: String -> String -> State Environment String
useNextRegister rtype str = do
    Environment file global local@(Local registers) <- get

    -- Note: This is guaranteed to succeed because we're searching through an infinite list.
    -- That being said, the register might not always be valid, but that's another problem.
    case find (`Map.notMember` registers) $ map ((rtype ++) . show) [0..] of
        Just register -> do
            put $ Environment file global $ Local $ Map.insert register str registers
            pure register

generateArgs :: [Var] -> State Environment [MIPSInstruction]
generateArgs [] = pure []
generateArgs (Var _ varName:args) = do
    reg <- useNextRegister "s" varName -- Use the s registers to store in case of function calls that would override.
    instr <- generateArgs args
    pure $ Inst OP_MOVE reg ("a" ++ tail reg) "" : instr

getRegsOfType :: String -> State Environment [String]
getRegsOfType rType = do
    Environment _ _ (Local registers) <- get
    pure $ filter (rType `isPrefixOf`) $ Map.keys registers

getRegister :: String -> State Environment String
getRegister varName = do
    Environment _ _ (Local registers) <- get
    pure $ fromMaybe (error ("Undefined reference to: " ++ varName)) $
              lookup varName $ map (\(a, b) -> (b, a)) $ Map.assocs registers

getNextLabel :: String -> State Environment String
getNextLabel labelType = do
    Environment file (Global labels funcs curFunc) local <- get

    let n = length $ filter (labelType `isPrefixOf`) labels
        newLabel = if n > 0 then
                        labelType ++ "_" ++ show n
                   else
                        labelType

    put $ Environment file (Global (newLabel:labels) funcs curFunc) local
    pure newLabel

funcLabel :: String -> State Environment (String, String)
funcLabel funcName = do
    funcLabel <- getNextLabel funcName
    funcEnd <- getNextLabel $ funcName ++ "_end"
    Environment file (Global labels funcs curFunc) local <- get
    put $ Environment file (Global labels (Map.insert funcName (funcLabel, funcEnd) funcs) curFunc) local
    pure (funcLabel, funcEnd)

getFuncLabel :: String -> State Environment (String, String)
getFuncLabel funcName = do
    Environment _ (Global _ funcs _) _ <- get
    pure $ fromMaybe (error ("Undefined reference to: " ++ funcName)) $ Map.lookup funcName funcs

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

setCurFunc :: String -> State Environment ()
setCurFunc curFunc = modify (\(Environment file (Global labels funcs _) local) -> Environment file (Global labels funcs curFunc) local)

getCurFunc :: State Environment String
getCurFunc = do
    Environment _ (Global _ _ curFunc) _ <- get
    pure curFunc
