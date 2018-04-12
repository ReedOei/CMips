module Optimizer where

import Control.Monad
import Control.Monad.State

import Data.List ((\\), isPrefixOf, findIndices, find, findIndex, nub)
import Data.Maybe (isNothing, mapMaybe, isJust, fromMaybe)

import Compiler.Types
import MIPSLanguage

import Util

import System.IO.Unsafe

optimize :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimize = findTemp [] >=>
           (untilM noChange (optimizeArith [] >=> optimizeResults >=> optimizeJumps) . pure)
           -- allocateRegisters >=>
           -- handleResSave

optimizeArith :: [MIPSInstruction] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeArith _ [] = pure []
optimizeArith prev (instr:instrs) = do
    let newInstr =
            if isArith instr then
                -- Don't need to check any more cases, because all arith will be instructions
                case instr of
                    -- Only c can be an immediate, for most things
                    -- Inst op a b c | commutes op ->
                    --     let cConst = resolveConstant prev c
                    --         bConst = resolveConstant prev b in
                    --         case (resolveConstant prev b, resolveConstant prev c) of
                    --             (Just bVal, Just cVal) -> Inst OP_LI a (show (compute op (read bVal) (read cVal))) ""
                    --             -- Transform "op a b c" into "op a c bVal", which is valid because op commutes.
                    --             (Just bVal, Nothing) -> replaceOperand b c (replaceOperand c bVal instr)
                    --             (Nothing, Just cVal) -> replaceOperand c cVal instr
                    --             _ -> instr

                    Inst op a b c -> replaceOperand c (fromMaybe c (resolveConstant prev c)) instr
            else
                case instr of
                    -- Moving a constant into a variable is also pointless.
                    Inst OP_MOVE a b c ->
                        case resolveConstant prev b of
                            Nothing -> instr
                            Just val -> Inst OP_LI a val ""
                    _ -> instr

    (:) <$> pure newInstr <*> optimizeArith (prev ++ [instr]) instrs

-- Finds out if a register is a constant.
-- If it is, replaces the register reference with the constant, if it is allowed in this position.
-- Otherwise, returns Nothing
resolveConstant :: [MIPSInstruction] -> String -> Maybe String
resolveConstant prev regName =
    case findIndex (hasOperand (== regName)) $ reverse prev of
        Just i ->
            let beforeI = reverse $ drop (i + 1) $ reverse prev
                hasCall = isJust $ find isCall $ take i $ reverse prev in
                case reverse prev !! i of
                    -- If there was a call and this isn't a saved value, then we can't rely on it being constant through the call.
                    _ | hasCall && not ("result_save" `isPrefixOf` regName)-> Nothing

                    -- Need to make sure we don't use li multiple times.
                    Inst OP_LI _ val _ | isNothing (find (hasOperand (== regName)) beforeI) -> Just val
                    Inst OP_MOVE dest source _ | dest == regName -> resolveConstant beforeI source
                    Inst op a b c | a == regName ->
                        -- Make sure both operands are also constants.
                        case (,) <$> (read <$> resolveConstant beforeI b)
                                 <*> (read <$> resolveConstant beforeI c) of
                            Just (bVal, cVal) -> Just $ show $ compute op bVal cVal
                            _ -> Nothing
                    _ -> Nothing
        Nothing -> Nothing

optimizeJumps :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeJumps = removeUselessJumps >=> removeUnusedLabels

removeUselessJumps :: [MIPSInstruction] -> State Environment [MIPSInstruction]
removeUselessJumps [] = pure []
removeUselessJumps (instr:instrs) =
    case branchTarget instr of
        Just branchName ->
            case getNext instrs of
                Just (Label labelName) | branchName == labelName ->
                    removeUselessJumps $ dropWhile (/= Label labelName) instrs
                _ -> (:) <$> pure instr <*> removeUselessJumps instrs
        Nothing -> (:) <$> pure instr <*> removeUselessJumps instrs

removeUnusedLabels :: [MIPSInstruction] -> State Environment [MIPSInstruction]
removeUnusedLabels instrs = pure $ filter go instrs
    where
        branchTargets = mapMaybe branchTarget instrs
        go (Label labelName)
            | labelName `elem` branchTargets = True
            | otherwise = False
        go _ = True

-- Returns the next instruction that is not empty or a comment
getNext :: [MIPSInstruction] -> Maybe MIPSInstruction
getNext = find go
    where
        go Inst{} = True
        go Label{} = True
        go _ = False

optimizeResults :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeResults = optimizeUnused [] >=> optimizeArgTemp >=> optimizeIdMove

optimizeIdMove :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeIdMove [] = pure []
optimizeIdMove (instr:instrs) =
    case instr of
        -- If we move a to itself, then what is the point.
        Inst OP_MOVE a b _ | a == b -> optimizeIdMove instrs
        _ -> (:) <$> pure instr <*> optimizeIdMove instrs

-- If we move an arg into a temp variable, we might as well just use the arg itself.
optimizeArgTemp :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeArgTemp [] = pure []
optimizeArgTemp (instr:instrs) =
    case instr of
        Inst OP_MOVE a b c | isRegType "a" b && isRegType "result_temp" a ->
            optimizeArgTemp $ map (replaceOperand a b) instrs
        _ -> (:) <$> pure instr <*> optimizeArgTemp instrs

-- Keep track of previous instructions because jumps may make it so that we execute stuff before the
-- instruction that is currently being processed.
optimizeUnused :: [MIPSInstruction] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeUnused _ [] = pure []
optimizeUnused prev (instr:instrs) =
    case instr of
        -- If we use this reg to store, then we assume that all memory locations are uses.
        Inst OP_SW _ _ _ -> (:) <$> pure instr <*> optimizeUnused (prev ++ [instr]) instrs
        Inst OP_SB _ _ _ -> (:) <$> pure instr <*> optimizeUnused (prev ++ [instr]) instrs
        _ -> case instResult instr of
                -- By default, v and a registers are always assumed used.
                Just regName | isRegType "v" regName || isRegType "a" regName -> (:) <$> pure instr <*> optimizeUnused (prev ++ [instr]) instrs

                -- Look at everything but this instruction (but obviously this instruction is relevant to itself)
                Just regName | regName `notElem` nub (concatMap instUses (prev ++ instrs)) -> optimizeUnused prev instrs
                _  -> (:) <$> pure instr <*> optimizeUnused (prev ++ [instr]) instrs

getUnusedRegisters :: [String] -> [String]
getUnusedRegisters regs = avail \\ regs
    where avail = map (("t" ++) . show) [0..9] ++ map (("s" ++) . show) [0..7]

-- Finds if result_save can be converted to result_temp
-- The list is all the registers we've examined so far.
findTemp :: [String] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
findTemp _ [] = pure []
findTemp examined (instr:instrs) = do
    (newInstr:newInstrs) <-
        case instr of
                Inst op a b c -> do
                    newInstr@(Inst op newA newB newC) <- foldM go instr $ filter (`notElem` examined) $ getOperands instr
                    let newInstrs = map (replaceOperand a newA . replaceOperand b newB . replaceOperand c newC) instrs
                    pure $ newInstr : newInstrs
                _ -> pure $ instr : instrs

    (:) <$> pure newInstr <*> findTemp (getOperands instr ++ examined) newInstrs
    where
        go instr a
            -- No JALs here, so safe to just use temp for this.
            | isNothing (find isCall (scope a instrs)) && "result_save" `isPrefixOf` a = do
                ref <- getRegRef a
                newReg <- useNextRegister "result_temp" ref
                freeRegister a -- We aren't using this anymore.
                pure $ replaceOperand a newReg instr
            | otherwise = pure instr

-- Finds the list of instructions for which this register must be in scope.
scope :: String -> [MIPSInstruction] -> [MIPSInstruction]
scope regName instrs = take lastIndex instrs
    where
        lastIndex =
            case findIndices (hasOperand (== regName)) instrs of
                [] -> 0
                indices -> last indices

-- Converts result_save and result_temp to s and t registers.
allocateRegisters :: [MIPSInstruction] -> State Environment [MIPSInstruction]
allocateRegisters [] = pure []
allocateRegisters (instr:instrs) = do
    newInstr <- foldM allocate instr $ getOperands instr
    mapM_ freeIfNotUsed $ getOperands instr

    (:) <$> pure newInstr <*> allocateRegisters instrs

    where
        allocate instr a
            | "result_save" `isPrefixOf` a || "result_temp" `isPrefixOf` a = do
                exists <- registerNameExists a

                reg <- if not exists then
                            if "result_save" `isPrefixOf` a then
                                useNextRegister "s" a
                            else
                                useNextRegister "t" a
                       else
                            getRegister a

                pure $ replaceOperand a reg instr
            | otherwise = pure instr

        freeIfNotUsed a
            | any (hasOperand (== a)) instrs = pure ()
            | otherwise = freeRegister a

outOfRange :: String -> Bool
outOfRange "" = False
outOfRange reg@(_:_) = all (`elem` "1234567890") (tail reg)
                    && (("s" `isPrefixOf` reg && read (tail reg) > 7) ||
                         "t" `isPrefixOf` reg && read (tail reg) > 9)

handleResSave :: [MIPSInstruction] -> State Environment [MIPSInstruction]
handleResSave instr =
    case findSplit (hasOperand outOfRange) instr of
        Nothing -> pure instr
        Just (beforeLoad, Inst op a b c, afterLoad) ->
            case getUnusedRegisters [a,b,c] of
                [] -> error "Fatal: Cannot figure out how to load registers from stack. All s and t registers are unavailable!"
                (reg:_) -> do
                    let saveReg = head $ filter outOfRange [a,b,c]

                    exists <- onStack saveReg

                    loadOffset <- if exists then
                                    getStackLoc saveReg
                                  else
                                    show <$> stalloc saveReg 4
                    offset <- stalloc (saveReg ++ "_temp") 4

                    let startInstr = [Inst OP_SW reg (show offset) "sp", Inst OP_LW reg loadOffset "sp"]
                    let endInstr = [Inst OP_SW reg loadOffset "sp", Inst OP_LW reg (show offset) "sp"]

                    let newInstr = replaceOperand saveReg reg (Inst op a b c)

                    -- Keep going until there is no more stack access to handle.
                    handleResSave $ beforeLoad ++ startInstr ++ [newInstr] ++ endInstr ++ afterLoad

