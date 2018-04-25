{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulator where

import Control.Lens (makeLenses, view, set, over)
import Control.Monad.State

import Data.Bits
import Data.Bits.Floating
import Data.List (findIndex, isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, isJust, isNothing)

import MIPSLanguage
import MIPSParser
import Util

import System.IO.Unsafe

data MIPSEnv = MIPSEnv
    { _registers :: Map String Int
    , _floatRegisters :: Map String Int -- Holds bit representation of float.
    , _floatCode :: Bool
    , _memory :: Map Int Int
    , _instrs :: [MIPSInstruction]
    , _labels :: Map String Int
    , _pc :: Int -- pc is the current instruction index.
    , _heap :: Int
    , _dataLocs :: Map String Int -- Starting address of data locations.
    , _constants :: Map String Int
    , _executed :: Int }
    deriving Show
makeLenses ''MIPSEnv

defaultVal :: String -> (String, Int)
defaultVal regName = (regName, 0)

emptyRegisters :: Map String Int
emptyRegisters = Map.fromList $ ("0", 0) : ("sp", 2147483647) : ("ra", 2147483647) :
                                map defaultVal (["at", "v0", "v1"] ++ argRegs ++ tempRegs ++ saveRegs ++ ["k0", "k1", "gp", "fp"])
    where
        argRegs = map (("a" ++) . show) [0..3]
        tempRegs = map (("t" ++) . show) [0..9]
        saveRegs = map (("s" ++) . show) [0..7]

emptyFloatRegisters :: Map String Int
emptyFloatRegisters = Map.fromList $ map (\i -> ("f" ++ show i, fromIntegral $ coerceToWord (0.0 :: Float))) [0..31]

makeConstants :: MIPSSection -> State MIPSEnv ()
makeConstants (MIPSSection "data" items) = mapM_ go items
    where
        go (name, kind, val) = do
            addr <- case kind of
                        "asciiz" -> putString val
                        "word" -> putWords $ map read $ words val
                        -- Must be a multiple of 4
                        "space" -> putWords $ replicate (read val `div` 4) 0
                        _ -> error $ "Unrecognized data kind: " ++ kind
            modify $ over constants (Map.insert name addr)

makeConstants _ = pure ()

makeEnv :: MIPSFile -> MIPSEnv
makeEnv (MIPSFile fname sections blocks) = execState (mapM_ makeConstants sections) baseState
    where
        -- Heap starts at 1 because NULL is 0.
        baseState = MIPSEnv emptyRegisters emptyFloatRegisters False Map.empty instrs labels 0 1 Map.empty Map.empty 0
        instrs = concat blocks
        labels = Map.fromList $ map (\(Label name, i) -> (name, i)) $ filter (isLabel . fst) $ zip instrs [0..]

execute :: MIPSFile -> ([String], MIPSEnv)
execute file@(MIPSFile _ _ blocks) =
    let (output, state) = runState (executeInstr =<< jump =<< labelDest "main") $ makeEnv file in
        (lines $ concat output, state)

current :: State MIPSEnv (Maybe MIPSInstruction)
current = do
    i <- view pc <$> get
    instrList <- view instrs <$> get

    if i >= length instrList then
        pure Nothing
    else
        pure $ Just $ instrList !! i

next :: State MIPSEnv (Maybe MIPSInstruction)
next = modify (over pc (+1)) >> current

jump :: Int -> State MIPSEnv (Maybe MIPSInstruction)
jump i = modify (set pc i) >> current

jumpLink :: Int -> State MIPSEnv (Maybe MIPSInstruction)
jumpLink i = do
    cur <- view pc <$> get
    setRegisterValue "ra" $ cur + 1
    jump i

labelDest :: String -> State MIPSEnv Int
labelDest labelName = do
    index <- Map.lookup labelName . view labels <$> get
    case index of
        Nothing -> error $ "Tried to jump to unknown label: " ++ labelName
        Just i -> pure i

registerValue :: String -> State MIPSEnv Int
registerValue regName =
    fromMaybe (error ("Unknown register: " ++ regName)) . Map.lookup regName . view registers <$> get

setRegisterValue :: String -> Int -> State MIPSEnv ()
setRegisterValue regName val = modify $ over registers (Map.insert regName val)

registerValueF :: String -> State MIPSEnv Int
registerValueF regName =
    fromMaybe (error ("Unknown float register: " ++ regName)) . Map.lookup regName . view floatRegisters <$> get

setRegisterValueF :: String -> Int -> State MIPSEnv ()
setRegisterValueF regName val = modify $ over floatRegisters (Map.insert regName val)

isNum :: String -> Bool
isNum = all (`elem` ("1234567890" :: String))

isFloat :: String -> Bool
isFloat = all (`elem` ("1234567890." :: String))

regOrNum :: String -> State MIPSEnv Int
regOrNum regName = do
    regVal <- Map.lookup regName . view registers <$> get
    case regVal of
        Just x -> pure x
        Nothing | isNum regName -> pure $ read regName
        _ -> error $ "Unknown register or non-numeric immediate: " ++ regName

regOrFloat :: String -> State MIPSEnv Int
regOrFloat regName = do
    regVal <- Map.lookup regName . view floatRegisters <$> get
    case regVal of
        Just x -> pure x
        Nothing | isFloat regName -> pure $ fromIntegral $ coerceToWord (read regName :: Float)
        _ -> error $ "Unknown float register or non-numeric immediate: " ++ regName

saveMemory :: Int -> Int -> State MIPSEnv ()
saveMemory address val = modify (over memory (Map.insert address val))

loadMemory :: Int -> State MIPSEnv Int
loadMemory 0 = error "Tried to load from NULL"
loadMemory address = fromMaybe 0 . Map.lookup address . view memory <$> get

modifyAndRun :: State MIPSEnv () -> State MIPSEnv [String]
modifyAndRun action = action >> next >>= executeInstr

loadNWords :: Int -> Int -> State MIPSEnv [Maybe Int]
loadNWords start n = do
    mem <- view memory <$> get
    pure $ map (`Map.lookup` mem) [start..start + n]

-- | Save a list of words in the first memory area large enough to continuously store it, returning the starting address.
putWords :: [Int] -> State MIPSEnv Int
putWords vals = do
    address <- putWords' =<< (view heap <$> get)
    modify $ set heap (address + length vals) -- Move heap up.
    mapM_ (uncurry saveMemory) $ zip [address..] vals

    pure address

    where
        putWords' address = do
            nextN <- loadNWords address (length vals)
            if all isNothing nextN then
                pure address
            else
                -- Just to after the next used slot.
                case findIndex isJust nextN of
                    Just i -> putWords' (address + i)
                    Nothing -> putWords' (address + 1)

putString :: String -> State MIPSEnv Int
putString str = putWords (map fromEnum str ++ [0]) -- Put \0 at the end.

loadString :: Int -> State MIPSEnv String
loadString address =
    condM (== 0) (loadMemory address) (const (pure "")) $ \c ->
        prependA (toEnum c) (loadString (address + 1))

-- | Execute a single instruction, then move on to the next instruction (wherever that is).
-- Output is a list of strings printed by the program we are running.
executeInstr :: Maybe MIPSInstruction -> State MIPSEnv [String]
executeInstr Nothing = pure [] -- We're done.
executeInstr (Just (instr@(Inst op a b c))) = do
    -- pcV <- view pc <$> get
    -- unsafePerformIO $ do
    --     print (pcV, instr)
    --     pure $ modify id
    modify $ over executed (+ 1)
    case op of
        OP_J -> executeInstr =<< jump =<< labelDest a
        OP_JR -> executeInstr =<< jump =<< registerValue a
        OP_JAL -> executeInstr =<< jumpLink =<< labelDest a
        OP_JALR -> executeInstr =<< jumpLink =<< registerValue a

        OP_BC1F -> do
            t <- view floatCode <$> get
            if not t then
                executeInstr =<< jump =<< labelDest a
            else
                executeInstr =<< next
        OP_BC1T -> do
            t <- view floatCode <$> get
            if t then
                executeInstr =<< jump =<< labelDest a
            else
                executeInstr =<< next

        OP_LW -> do
            setRegisterValue a =<< loadMemory =<< ((read b +) <$> registerValue c)

            executeInstr =<< next
        OP_LB -> modifyAndRun (setRegisterValue a =<< loadMemory =<< ((read b +) <$> registerValue c))

        OP_SW -> do
            join (saveMemory <$> ((read b +) <$> registerValue c) <*> registerValue a)
            executeInstr =<< next
        OP_SB -> modifyAndRun (join (saveMemory <$> ((read b +) <$> registerValue c) <*> registerValue a))

        OP_LI -> modifyAndRun (setRegisterValue a (read b))
        OP_MOVE -> modifyAndRun (setRegisterValue a =<< registerValue b)
        OP_NOT -> modifyAndRun (setRegisterValue a =<< (complement <$> registerValue b))

        OP_MOVS -> modifyAndRun (setRegisterValueF a =<< registerValueF b)
        OP_MTC1 -> modifyAndRun (setRegisterValueF b =<< registerValue a)
        OP_MFC1 -> modifyAndRun (setRegisterValue a =<< registerValueF b)
        OP_CVT_W_S -> do
            bitVal <- coerceToFloat . fromIntegral <$> registerValueF b
            modifyAndRun (setRegisterValueF a (floor (bitVal :: Float)))
        OP_CVT_S_W -> do
            floatVal <- fromIntegral <$> registerValueF b
            let intVal = fromIntegral $ coerceToWord (floatVal :: Float) :: Int
            modifyAndRun (setRegisterValueF a intVal)

        OP_LIS -> modifyAndRun (setRegisterValueF a (fromIntegral (coerceToWord (read b :: Float))))

        OP_LS -> do
            oldVal <- loadMemory =<< ((read b +) <$> registerValue c)
            modifyAndRun $ setRegisterValueF a oldVal

        OP_SS -> modifyAndRun (join (saveMemory <$> ((read b +) <$> registerValue c) <*> registerValueF a))


        OP_LA -> do
            -- Figure out what kind of thing it is.
            label <- Map.lookup b . view labels <$> get
            dataLocs <- Map.lookup b . view dataLocs <$> get
            constant <- Map.lookup b . view constants <$> get

            -- Find which one of them exists
            case (label, dataLocs, constant) of
                (Just labelLoc, _, _) -> modifyAndRun (setRegisterValue a labelLoc)
                (_, Just dataLoc, _) -> modifyAndRun (setRegisterValue a dataLoc)
                (_, _, Just constantLoc) -> modifyAndRun (setRegisterValue a constantLoc)
                _ -> executeInstr =<< next

        SYSCALL -> do
            syscallNum <- registerValue "v0"

            output <- case syscallNum of
                            10 -> do
                                -- Free memory
                                modify $ set heap 1
                                pure []
                            9 -> do
                                -- Allocate memory
                                amount <- registerValue "a0"
                                setRegisterValue "v0" =<< (view heap <$> get)
                                modify $ over heap (+ amount)
                                pure []

                            4 -> do
                                --print string
                                str <- loadString =<< registerValue "a0"
                                pure [str]

                            2 -> do
                                -- print float
                                v :: Float <- coerceToFloat . fromIntegral <$> registerValueF "f12"
                                pure [show v]

                            1 -> do
                                -- print it.
                                v <- show <$> registerValue "a0"
                                pure [v]

                            _ -> error $ "Got unknown syscall number: " ++ show syscallNum

            (++) <$> pure output <*> (executeInstr =<< next)

        _ | isBranch instr -> do
            t <- checkBranch op <$> registerValue a <*> regOrNum b
            if t then
                executeInstr =<< jump =<< labelDest c
            else
                executeInstr =<< next

        _ | isBranchFloat instr -> do
            t <- checkBranchFloat op <$> registerValueF a <*> regOrFloat b

            modifyAndRun $ modify $ set floatCode t

        _ | isArith instr -> modifyAndRun (setRegisterValue a =<< (compute op <$> registerValue b <*> regOrNum c))
        _ | isArithFloat instr -> do
            bVal <- coerceToFloat . fromIntegral <$> registerValueF b
            cVal <- coerceToFloat . fromIntegral <$> registerValueF c

            modifyAndRun (setRegisterValueF a (fromIntegral (coerceToWord (computeFloat op bVal cVal))))
executeInstr _ = do
    modify $ over executed (+ 1)
    executeInstr =<< next

