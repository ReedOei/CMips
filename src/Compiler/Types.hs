{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Compiler.Types where

import Control.Lens ((^.), view, over, makeLenses, set)
import Control.Monad.State

import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)

import CLanguage
import MIPSLanguage
import Util

data Warning where
    Warning :: forall t. PrettyPrint t => Context t -> String -> Warning

instance Show Warning where
    show (Warning context message) =
        case context ^. funcName of
            Nothing -> prettyPrint (context ^. val) ++ ": " ++ message
            Just fname -> fname ++ ": " ++ prettyPrint (context ^. val) ++ ": " ++ message

data CompileOptions = CompileOptions
    { _optimizeLevel :: Int
    , _useInlining :: Bool }
    deriving Show
makeLenses ''CompileOptions

defaultCompileOptions = CompileOptions 1 True

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
data Global = Global
    { _labels :: [String]
    , _funcs :: Map String (String, String)
    , _curFunc :: String }
    deriving Show
makeLenses ''Global

-- Contains:
-- Amount of stack space used.
-- Map of registers to variable names.
data Local = Local
    { _stack :: Int
    , _registers :: Map String String
    , _stackLocs :: Map String String }
    deriving Show
makeLenses ''Local

data Environment = Environment
    { _file :: CFile
    , _dataSections :: Data
    , _global :: Global
    , _local :: Local
    , _compiled :: Map String [MIPSInstruction] -- The compiled assembly for all functions that have been compiled so far.
    , _warnings :: [Warning]
    , _compileOptions :: CompileOptions }
    deriving Show
makeLenses ''Environment

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
generateArgs (Var _ varName:args) = do
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

