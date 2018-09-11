{-# LANGUAGE TemplateHaskell #-}

module Compiler.Types where

import Control.Lens ((^.), view, over, makeLenses, set)
import Control.Monad.State

import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)

import CLanguage
import MIPSLanguage
import Types
import Util

saveKData :: String -> String -> State Environment String
saveKData dataType dataVal = do
    Data d kd <- view dataSections <$> get

    let name = "kd" ++ show (length kd)
    let newD = DataElement name dataType dataVal : kd

    modify $ set dataSections $ Data d newD
    pure name

saveData :: String -> String -> State Environment String
saveData dataType dataVal = do
    Data d kd <- view dataSections <$> get

    let name = "d" ++ show (length d)
    let newD = DataElement name dataType dataVal : d

    modify $ set dataSections $ Data newD kd
    pure name

useNextRegister :: String -> String -> State Environment String
useNextRegister rtype str = do
    curRegs <- view (local . registers) <$> get

    -- Note: This is guaranteed to succeed because we're searching through an infinite list.
    -- That being said, the register might not always be valid, but that's another problem.
    case find ((`Map.notMember` curRegs) . (rtype ++) . show) [0..] of
        Just regNum -> do
            modify $ over (local . registers) $ Map.insert (rtype ++ show regNum) str
            pure $ rtype ++ show regNum

freeRegister :: String -> State Environment ()
freeRegister name = do
    exists <- registerNameExists name

    if exists then do
        regName <- getRegister name

        modify $ over (local . registers) $ Map.delete regName
        modify $ over (local . stackLocs) $ Map.delete regName
    else
        pure ()


regList = "0" : "sp" : "ra" : ["at", "v0", "v1"] ++ argRegs ++ tempRegs ++ saveRegs ++ floatRegs ++ ["k0", "k1", "gp", "fp"]
    where
        argRegs = map (("a" ++) . show) [0..3]
        tempRegs = map (("t" ++) . show) [0..9]
        saveRegs = map (("s" ++) . show) [0..7]
        floatRegs = map (("f" ++) . show) [0..31]

isRegister :: String -> Bool
isRegister r = r `elem` regList

isRegType :: String -> String -> Bool
isRegType rtype reg = rtype `isPrefixOf` reg && all (`elem` ("1234567890" :: String)) (drop (length rtype) reg)

generateArgs :: [Var] -> State Environment [MIPSInstruction]
generateArgs [] = pure []
generateArgs (Var _ _ varName:args) = do
    reg <- useNextRegister "result_save" varName -- Use a saved register to store in case of function calls that would override.
    aReg <- useNextRegister "a" $ varName ++ "arg"
    instr <- generateArgs args
    pure $ Inst OP_MOVE reg aReg "" : instr

getRegsOfType :: String -> State Environment [String]
getRegsOfType rType = filter (rType `isPrefixOf`) . Map.keys . view (local . registers) <$> get

flipLookup k = lookup k . map (\(a, b) -> (b, a)) . Map.assocs

registerNameExists :: String -> State Environment Bool
registerNameExists regName = isJust . flipLookup regName . view (local . registers) <$> get

getRegister :: String -> State Environment String
getRegister varName = do
    curRegs <- view (local . registers) <$> get
    fromMaybe (error ("Undefined reference to register: " ++ varName ++ " (" ++ show curRegs ++ ")")) .
        flipLookup varName . view (local . registers) <$> get

getRegRef :: String -> State Environment String
getRegRef regName = do
    curRegs <- view (local . registers) <$> get
    fromMaybe (error ("Undefined reference to register name: " ++ regName ++ " (" ++ show curRegs ++ ")")) .
        Map.lookup regName . view (local . registers) <$> get

onStack :: String -> State Environment Bool
onStack name = isJust . Map.lookup name . view (local . stackLocs) <$> get

getStackLoc :: String -> State Environment String
getStackLoc name =
    fromMaybe (error ("Undefined reference to stack location: " ++ name)) .
        Map.lookup name . view (local . stackLocs) <$> get

getNextLabel :: String -> State Environment String
getNextLabel labelType = do
    curLabels <- view (global . labels) <$> get

    let n = length $ filter (labelType `isPrefixOf`) curLabels
        newLabel = if n > 0 then
                        labelType ++ "_" ++ show n
                   else
                        labelType
    modify $ over (global . labels) (newLabel:)

    pure newLabel

getFuncNameByLabel :: String -> State Environment (Maybe String)
getFuncNameByLabel funcLabel = lookup funcLabel . map (\(k, (fLabel, _)) -> (fLabel, k)) . Map.assocs . view (global . funcs) <$> get

funcLabel :: String -> State Environment (String, String)
funcLabel funcName = do
    funcLabel <- getNextLabel funcName
    funcEnd <- getNextLabel $ funcName ++ "_end"

    modify $ over (global . funcs) $ Map.insert funcName (funcLabel, funcEnd)

    pure (funcLabel, funcEnd)

getFuncLabel :: String -> State Environment (Maybe (String, String))
getFuncLabel funcName = Map.lookup funcName . view (global . funcs) <$> get

purgeRegType :: String -> Local -> Local
purgeRegType rType (Local stack registers stackLocs) = Local stack (Map.filterWithKey (\k _ -> not (rType `isPrefixOf` k)) registers) stackLocs

purgeRegTypeEnv :: String -> Environment -> Environment
purgeRegTypeEnv rType = over local (purgeRegType rType)

purgeRegTypesEnv :: [String] -> Environment -> Environment
purgeRegTypesEnv rTypes env = foldr purgeRegTypeEnv env rTypes

emptyEnvironment :: Environment
emptyEnvironment = Environment (CFile "" []) (Data [] []) (Global [] Map.empty "") (Local 0 Map.empty Map.empty) Map.empty [] defaultCompileOptions

newEnvironment :: CompileOptions -> CFile -> Environment
newEnvironment opts file = Environment file (Data [] []) (Global [] Map.empty "") (Local 0 Map.empty Map.empty) Map.empty [] opts

resetLocal :: Environment -> Environment
resetLocal = set local $ Local 0 Map.empty Map.empty

setCurFunc :: String -> State Environment ()
setCurFunc newFunc = modify $ set (global . curFunc) newFunc

getCurFunc :: State Environment String
getCurFunc = view (global . curFunc) <$> get

stalloc :: String -> Int -> State Environment Int
stalloc name amount = do
    initStack <- view (local . stack) <$> get

    modify $ over (local . stack) (+ amount)
    modify $ over (local . stackLocs) $ Map.insert name (show initStack)

    pure initStack

