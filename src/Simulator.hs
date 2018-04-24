{-# LANGUAGE TemplateHaskell #-}

module Simulator where

import Control.Lens (makeLenses, view, set, over)
import Control.Monad.State

import Data.Bits
import Data.List (findIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, isJust, isNothing)

import MIPSLanguage
import MIPSParser
import Util

import System.IO.Unsafe

data MIPSEnv = MIPSEnv
    { _registers :: Map String Int
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
        baseState = MIPSEnv emptyRegisters Map.empty instrs labels 0 1 Map.empty Map.empty 0
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

isNum :: String -> Bool
isNum = all (`elem` ("1234567890" :: String))

regOrNum :: String -> State MIPSEnv Int
regOrNum regName = do
    regVal <- Map.lookup regName . view registers <$> get
    case regVal of
        Just x -> pure x
        Nothing | isNum regName -> pure $ read regName
        _ -> error $ "Unknown register or non-numeric immediate: " ++ regName

setRegisterValue :: String -> Int -> State MIPSEnv ()
setRegisterValue regName val = modify $ over registers (Map.insert regName val)

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

        OP_LW -> do
            setRegisterValue a =<< loadMemory =<< ((read b +) <$> registerValue c)

            executeInstr =<< next
        OP_LB -> modifyAndRun (setRegisterValue a =<< loadMemory =<< ((read b +) <$> registerValue c))

        OP_SW -> do
            -- if a == "ra" then do
            --     regs <- view registers <$> get
            --     unsafePerformIO $ do
            --         print regs
            --         pure $ modify id
            --     regs <- view memory <$> get
            --     unsafePerformIO $ do
            --         print regs
            --         pure $ modify id
            -- else
            --     pure ()
            join (saveMemory <$> ((read b +) <$> registerValue c) <*> registerValue a)
            executeInstr =<< next
        OP_SB -> modifyAndRun (join (saveMemory <$> ((read b +) <$> registerValue c) <*> registerValue a))

        OP_LI -> modifyAndRun (setRegisterValue a (read b))
        OP_MOVE -> modifyAndRun (setRegisterValue a =<< registerValue b)
        OP_NOT -> modifyAndRun (setRegisterValue a =<< (complement <$> registerValue b))

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

        _ | isArith instr -> modifyAndRun (setRegisterValue a =<< (compute op <$> registerValue b <*> regOrNum c))
executeInstr _ = do
    modify $ over executed (+ 1)
    executeInstr =<< next

