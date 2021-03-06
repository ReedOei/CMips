module Optimizer where

import Control.Lens (view, set)

import Control.Monad
import Control.Monad.State

import Data.List ((\\), isPrefixOf, findIndices, find, findIndex, nub)
import Data.Maybe (isNothing, mapMaybe, isJust, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

import Compiler.Types
import MIPSLanguage
import MIPSParser
import Types
import Util

optimize :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimize instrs = do
    optLevel <- view (compileOptions . optimizeLevel) <$> get

    if optLevel > 0 then
        if optLevel == 1 then
            findTemp [] instrs >>=
                   -- -- Repeatedly apply optimizations until there are no changes in the code.
                   (untilM noChange (optimizeArith [] >=>
                                     optimizeFloats >=>
                                     optimizeJumps) . pure)
        else if optLevel == 2 then
            findTemp [] instrs >>=
                   -- -- Repeatedly apply optimizations until there are no changes in the code.
                   (untilM noChange (optimizeArith [] >=>
                                     optimizeResults >=>
                                     optimizeFloats >=>
                                     optimizeJumps) . pure)
        else
            optimizeCalls instrs >>=
            findTemp [] >>=
                   -- -- Repeatedly apply optimizations until there are no changes in the code.
                   (untilM noChange (optimizeArith [] >=>
                                     optimizeResults >=>
                                     optimizeFloats >=>
                                     optimizeJumps) . pure)
    else
        pure instrs

allocate :: [MIPSInstruction] -> State Environment [MIPSInstruction]
allocate = allocateRegisters >=> handleResSave

optimizeCalls :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeCalls instrs = do
    inline <- view (compileOptions . useInlining) <$> get
    if inline then
        optimizeInlining [] instrs
    else
        pure instrs

-- | Tries to inline functions.
--   Currently, this can only be done if the compiled assembly of the function meets the following criteria:
--
--   1. The only jump is jr $ra.
--   2. Only registers used are $a and $v registers.
--   3. No labels other than the function name label.
--
--   Additionally, cannot be done to jalr's, because it is not known at compile time whether they could be inline.
optimizeInlining :: [MIPSInstruction] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeInlining _ [] = pure []
optimizeInlining prevInstr (instr@(Inst OP_JAL funcLabel _ _):instrs) = do
    funcName <- fromMaybe (error ("Encountered unknown function label while inlining: '" ++ funcLabel ++ "'")) <$> getFuncNameByLabel funcLabel
    curFuncName <- getCurFunc

    -- Make sure we don't try to inline ourselves (because we haven't compiled ourselves yet).
    if funcName /= curFuncName then do
        compiledFunc <- Map.lookup funcName . view compiled <$> get

        let compiledInstrs = fromMaybe (error ("Encountered uncompiled function while inlining: '" ++ funcName ++ "'")) compiledFunc

        if all easyInlinable compiledInstrs then do
            -- Delete the call and copy the compiled body in. Remove all jumps and labels from the inlined functions.
            let replacement = filter (\i -> not (isLabel i) && not (isJump i)) compiledInstrs
            (++) <$> pure replacement <*> optimizeInlining (prevInstr ++ replacement) instrs
        else
            -- unsafePerformIO $ do
            --     putStrLn $ "Can we aggressively inline a call to " ++ funcName ++ ": " ++ show (all inlinable compiledInstrs)
            --     pure $ pure ()

            -- See if seems like it's worth it to inline.
            if all inlinable compiledInstrs then do
                deallocated <- filter (\i -> not (is OP_JR i || isFuncLabel i)) <$> mapM deallocateRegisters compiledInstrs

                modify $ set (compileOptions . useInlining) False -- Turn off inlining so we don't get into a loop

                optimizedSelf <- optimize (prevInstr ++ deallocated ++ instrs)

                modify $ set (compileOptions . useInlining) True -- Turn inlining back on.

                if length optimizedSelf < length (prevInstr ++ [instr] ++ instrs ++ compiledInstrs) then
                    (++) <$> pure deallocated <*> optimizeInlining (prevInstr ++ deallocated) instrs
                else
                    prependA instr $ optimizeInlining (prevInstr ++ [instr]) instrs
            else
                prependA instr $ optimizeInlining (prevInstr ++ [instr]) instrs
    else
        prependA instr $ optimizeInlining (prevInstr ++ [instr]) instrs

    where
        easyInlinable (Inst OP_JR "ra" "" "") = True
        easyInlinable instr@(Inst op a b c)
            | isJump instr || isCall instr = False
            | otherwise = all (\r -> "a" `isPrefixOf` r || "v" `isPrefixOf` r) [a,b,c]
        easyInlinable (Label labelName) = labelName == funcLabel
        easyInlinable _ = True

        isFuncLabel (Label labelName) = labelName == funcLabel
        isFuncLabel _ = False

        deallocateRegisters instr@(Inst op a b c) = do
            newOps <- mapM dealloc [a,b,c]
            pure $ setOperands instr newOps
            where
                dealloc r
                    -- Make sure they get allocate to the same registers as before.
                    | isRegType "t" r = do
                        exists <- registerNameExists r
                        if exists then
                            getRegister r
                        else
                            useNextRegister "result_temp" r
                    | isRegType "f" r = do
                        exists <- registerNameExists r
                        if exists then
                            getRegister r
                        else
                            useNextRegister "result_float" r
                    | otherwise = pure r
        deallocateRegisters i = pure i

        inlinable (Inst OP_JR "ra" "" "") = True
        -- Can't do it if there are saved registers, calls, or stack management.
        inlinable instr@(Inst op a b c)
            | isCall instr = False
            | otherwise = all (\r -> r /= "sp" && not (isRegType "s" r)) [a,b,c]
        inlinable instr@(Label labelName) = labelName == funcLabel
        inlinable _ = True

optimizeInlining prevInstr (i:instrs) = prependA i $ optimizeInlining (prevInstr ++ [i]) instrs

isNum :: String -> Bool
isNum = all (`elem` ("1234567890" :: String))

optimizeArith :: [MIPSInstruction] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeArith _ [] = pure []
optimizeArith prev (instr:instrs) = do
    let newInstr =
            if isArith instr then
                -- Don't need to check any more cases, because all arith will be instructions
                case instr of
                    -- If we modify ourselves, we'll assume that we're not constant (for things like i++ in a loop),
                    -- even though it is possible that we are.
                    Inst op a b c | a == b || a == c -> instr

                    -- Arithmetic identities.
                    Inst OP_ADD a b "0" -> Inst OP_MOVE a b "" -- x+0 = x
                    Inst OP_SUB a b "0" -> Inst OP_MOVE a b "" -- x-0 = x
                    Inst OP_DIV a b "1" -> Inst OP_MOVE a b "" -- x/1 = x
                    Inst OP_MUL a b "1" -> Inst OP_MOVE a b "" -- 1*x = x
                    Inst OP_MUL a b "0" -> Inst OP_LI a "0" "" -- 0*x = 0

                    -- -- Only c can be an immediate, for most things, but not if op commutes.
                    Inst op a b c | commutes op ->
                        let cConst = resolveConstant prev c
                            bConst = resolveConstant prev b in
                            case (resolveConstant prev b, resolveConstant prev c) of
                                (Just bVal, Just cVal) -> Inst OP_LI a (show (compute op (read bVal) (read cVal))) ""
                                -- Transform "op a b c" into "op a c bVal", which is valid because op commutes.
                                (Just bVal, Nothing) -> replaceOperand b c (replaceOperand c bVal instr)
                                (Nothing, Just cVal) -> replaceOperand c cVal instr
                                _ -> instr

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
resolveConstant prev regName
    | isNum regName = Just regName
    | otherwise =
        case findIndex (hasOperand (== regName)) $ reverse prev of
            Just i ->
                let beforeI = reverse $ drop (i + 1) $ reverse prev
                    hasCall = isJust $ find isCall $ take i $ reverse prev
                    hasBranching = any (\i -> isJump i || isLabel i) $ take i $ reverse prev in
                    case reverse prev !! i of
                        -- Make sure we don't optimize away self modifying things.
                        Inst op a b c | a == regName && (a == b || a == c) -> Nothing

                        -- If there was a call and this isn't a saved value, then we can't rely on it being constant through the call.
                        _ | (hasCall && not ("result_save" `isPrefixOf` regName)) || hasBranching -> Nothing

                        -- Need to make sure we don't use li multiple times.
                        Inst OP_LI _ val _ | isNothing (find (hasOperand (== regName)) beforeI) -> Just val
                        Inst OP_MOVE dest source _ | dest == regName -> resolveConstant beforeI source
                        instr@(Inst op a b c) | isArith instr && a == regName ->
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
optimizeResults = optimizeUnused [] >=>
                  optimizeArgTemp >=> optimizeIdMove >=> optimizeMovedResults >=> optimizeAlias

-- | Checks the scope of a register to make sure there is no jumps, branches, calls, etc. in it.
-- Utility for optimizing.
scopeSafe :: String -> [MIPSInstruction] -> Bool
scopeSafe regName instrs =
    let s = scope regName instrs in
        not (any isLabel s) && not (any isJump s) && not (any isCall s)

-- | Counts how many times the register is used in the given instructions
uses :: String -> [MIPSInstruction] -> Int
uses regName = length . filter (hasOperand (== regName))

-- | Optimizations related to floats, mostly related to moving to/from coprocessor registers
optimizeFloats :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeFloats = optimizeFloatMove >=> optimizeMovedResultsF

-- | Optimize moving from, then back into float registers.
-- Example:
--
-- mfc1 $t0, $f0
-- mtc1 $t0, $f1
--
-- becomes:
--
-- mov.s $f1, $f0
optimizeFloatMove :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeFloatMove [] = pure []
optimizeFloatMove (instr@(Inst OP_MFC1 dest source _):instrs) =
    case find moveTo instrs of
        Just moveToInst@(Inst OP_MTC1 _ finalDest _)
            | scopeSafe dest (instr:instrs) && uses dest instrs == 1 ->
                prependA (Inst OP_MOVS finalDest source "") $ optimizeFloatMove $ filter (/= moveToInst) instrs
        _ -> prependA instr $ optimizeFloatMove instrs

    where
        moveTo (Inst OP_MTC1 moveSource _ _) = moveSource == dest
        moveTo _ = False
optimizeFloatMove (i:instrs) = prependA i $ optimizeFloatMove instrs

-- | Optimizes when the result of an instruction isn't used for anything other than being moved into another register.
-- Example:
--
-- jal f
-- move $t0, $v0
-- move $a0, $t0
--
-- becomes:
--
-- jal f
-- move $a0, $v0
optimizeMovedResultsF :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeMovedResultsF [] = pure []
optimizeMovedResultsF (instr@(Inst _ origA origB origC):instrs)
    | isFloatInstr instr =
        case instResult instr of
            -- Make sure there aren't any labels or jumps in scope, because then we'd have to predict the control flow.
            Just res | scopeSafe res (instr:instrs) -> do
                let allUses = find (hasOperand (== res)) instrs

                -- Find next move that moves 'res' into something else
                case find (go res) instrs of
                    Just (moveInstr@(Inst OP_MOVS a b _)) ->
                        -- Only optimize if there is only one other use (i.e., the move we found)
                        if length allUses > 1 || any (/= moveInstr) allUses then
                            prependA instr $ optimizeMovedResultsF instrs
                        else do
                            -- Only change the first operand.
                            let newInstr = setOperands instr [a, origB, origC]
                            -- Delete that move instruction, it's unnecessary now
                            prependA newInstr $ optimizeMovedResultsF $ filter (/= moveInstr) instrs

                    Nothing -> prependA instr $ optimizeMovedResultsF instrs
            _ -> prependA instr $ optimizeMovedResultsF instrs
    | otherwise = prependA instr $ optimizeMovedResultsF instrs

    where
        go res (Inst OP_MOVS a b _) = b == res
        go _ _ = False

optimizeMovedResultsF (i:instrs) = prependA i $ optimizeMovedResultsF instrs

-- | Optimize a move into a destination, when the destination is only used once.
-- Example:
--
-- move $t0, $t1
-- sw $t0, 0($t2)
--
-- becomes:
--
-- sw $t1, 0($t2)
optimizeAlias :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeAlias [] = pure []
optimizeAlias (instr@(Inst OP_MOVE dest source _):instrs)
    -- Can't do alias stuff with these, because they are special names.
    | "a" `isPrefixOf` dest || "v" `isPrefixOf` dest = prependA instr $ optimizeAlias instrs
    | otherwise = case filter (hasOperand (== dest)) instrs of
                    -- Make sure there are no uses of dest across labels, jumps, or function calls.
                    -- and make sure that there are no writes to source.
                    allUses@(_:_) | scopeSafe dest (instr:instrs) &&
                                    -- Check that we don't write to the source, this causes an issue with the renaming
                                    -- But if the write to the source is after we're done using dest, then it doesn't matter anymore.
                                    not (any (hasResult source) (init (scope dest (instr:instrs)))) ->
                        -- If it's safe, we'll just delete this move and replace the dest with the source from now on.
                        optimizeAlias $ map (replaceOperand dest source) instrs

                    _ -> prependA instr $ optimizeAlias instrs
    where
        hasResult regName instr =
            case instResult instr of
                Nothing -> False
                Just r -> r == regName

optimizeAlias (i:instrs) = prependA i $ optimizeAlias instrs

-- | Optimizes when the result of an instruction isn't used for anything other than being moved into another register.
-- Example:
--
-- jal f
-- move $t0, $v0
-- move $a0, $t0
--
-- becomes:
--
-- jal f
-- move $a0, $v0
optimizeMovedResults :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeMovedResults [] = pure []
optimizeMovedResults (instr@(Inst _ origA origB origC):instrs) =
    case instResult instr of
        -- Make sure there aren't any labels or jumps in scope, because then we'd have to predict the control flow.
        Just res | scopeSafe res (instr:instrs) -> do
            let allUses = find (hasOperand (== res)) instrs

            -- Find next move that moves 'res' into something else
            case find (go res) instrs of
                Just (moveInstr@(Inst OP_MOVE a b _)) ->
                    -- Only optimize if there is only one other use (i.e., the move we found)
                    if length allUses > 1 || any (/= moveInstr) allUses then
                        prependA instr $ optimizeMovedResults instrs
                    else do
                        -- Only change the first operand.
                        let newInstr = setOperands instr [a, origB, origC]
                        -- Delete that move instruction, it's unnecessary now
                        prependA newInstr $ optimizeMovedResults $ filter (/= moveInstr) instrs

                Nothing -> prependA instr $ optimizeMovedResults instrs
        _ -> prependA instr $ optimizeMovedResults instrs

    where
        go res (Inst OP_MOVE a b _) = b == res
        go _ _ = False

optimizeMovedResults (i:instrs) = prependA i $ optimizeMovedResults instrs

optimizeIdMove :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeIdMove [] = pure []
optimizeIdMove (instr:instrs) =
    case instr of
        -- If we move a to itself, then what is the point.
        Inst OP_MOVE a b _ | a == b -> optimizeIdMove instrs
        Inst OP_MOVS a b _ | a == b -> optimizeIdMove instrs
        _ -> (:) <$> pure instr <*> optimizeIdMove instrs

-- If we move an arg into a temp variable, we might as well just use the arg itself.
optimizeArgTemp :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimizeArgTemp [] = pure []
optimizeArgTemp (instr:instrs) =
    case instr of
        Inst OP_MOVE a b c |
            -- We also need to make sure that there isn't any usage of the a register as an actual argument in the scope of the temp
            -- register, otherwise we would overwrite it.
            isRegType "a" b && isRegType "result_temp" a &&
            isNothing (find (hasOperand (== b)) (init (scope a (instr:instrs)))) ->
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
                Just regName
                -- If this register isn't used and no calls, just loaded (can happen with inlining), then ignore it.
                             -- let ws = nextWriteScope regName in
                             --   (isRegType "v" regName || isRegType "a" regName) &&
                             --   not (any isCall ws || any isJump ws || any isLabel ws) &&
                             --   not (any (hasOperand (== regName)) ws) -> optimizeUnused prev instrs
                -- By default, v, a, and result_float registers are always assumed used.
                             | isRegType "v" regName ||
                               isRegType "a" regName ||
                               isRegType "f" regName || -- Have to do this in addition to result float because we need f12 for printing floats.
                               isRegType "result_float" regName -> prependA instr $ optimizeUnused (prev ++ [instr]) instrs

                -- Look at everything but this instruction (but obviously this instruction is relevant to itself)
                Just regName | regName `notElem` nub (concatMap instUses (prev ++ instrs)) -> optimizeUnused prev instrs
                _  -> prependA instr $ optimizeUnused (prev ++ [instr]) instrs
    where
        nextWriteScope regName =
            case findIndex (\i -> fromMaybe False $ (regName ==) <$> instResult i) instrs of
                Nothing -> instrs -- There is no next write, so this register is in scope for all of the instructions.
                Just i -> take (i - 1) instrs

getUnusedRegisters :: [String] -> [String]
getUnusedRegisters regs = avail \\ regs
    where avail = map (("t" ++) . show) [0..9] ++ map (("s" ++) . show) [0..7]

-- Finds if result_save can be converted to result_temp
-- The list is all the registers we've examined so far.
findTemp :: [String] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
findTemp _ [] = pure []
findTemp examined (instr:instrs) = do
    res <-
        case instr of
                Inst op a b c -> do
                    v <- foldM go instr $ filter (`notElem` examined) $ getOperands instr
                    pure $ case v of
                        newInstr@(Inst op newA newB newC) ->
                            let newInstrs = map (replaceOperand a newA . replaceOperand b newB . replaceOperand c newC) instrs in
                            newInstr : newInstrs
                        _ -> instr:instrs
                _ -> pure $ instr : instrs

    case res of
        newInstr:newInstrs -> (:) <$> pure newInstr <*> findTemp (getOperands instr ++ examined) newInstrs
        [] -> pure []
    where
        go instr a
            -- No JALs or JALRs here, so safe to just use temp for this.
            -- Check for init scope because if the very last instruction is jalr $t0, and we don't need t0 after that, then it's fine to use
            -- a temp for that.
            | scopeSafe a (instr:instrs) && "result_save" `isPrefixOf` a = do
                    ref <- getRegRef a
                    newReg <- useNextRegister "result_temp" ref
                    freeRegister a -- We aren't using this anymore.
                    pure $ replaceOperand a newReg instr
            | otherwise = pure instr

slice from to xs = take (to - from + 1) (drop from xs)

-- Finds the list of instructions for which this register must be in scope.
scope :: String -> [MIPSInstruction] -> [MIPSInstruction]
scope regName instrs = slice firstIndex lastIndex instrs
    where
        (firstIndex, lastIndex) =
            case findIndices (hasOperand (== regName)) instrs of
                [] -> (0, 0)
                indices -> (head indices, last indices)

-- Converts result_save to s registers, result_temp to t registers, and result_float to f registers (but ignores 12 to save for printing).
allocateRegisters instrs = allocateRegisters' instrs instrs

allocateRegisters' :: [MIPSInstruction] -> [MIPSInstruction] -> State Environment [MIPSInstruction]
allocateRegisters' _ [] = pure []
allocateRegisters' allInstrs (instr:instrs) = do
    newInstr <- foldM allocate instr $ getOperands instr
    mapM_ freeIfNotUsed $ getOperands instr

    (:) <$> pure newInstr <*> allocateRegisters' allInstrs instrs

    where
        allocate instr a
            | "result_save" `isPrefixOf` a ||
              "result_temp" `isPrefixOf` a ||
              "result_float" `isPrefixOf` a = do
                exists <- registerNameExists a

                reg <- if not exists then
                            if "result_save" `isPrefixOf` a then
                                useNextRegister "s" a
                            else if "result_float" `isPrefixOf` a then
                                useNextRegister "f" a
                            else
                                useNextRegister "t" a
                       else
                            getRegister a

                pure $ replaceOperand a reg instr
            | otherwise = pure instr

        freeIfNotUsed a
            -- If there are any labels in the scope of a, we can't guarantee there won't be jumps, so to be on the safe side,
            -- don't free a.
            | any isLabel (scope a allInstrs) || any (hasOperand (== a)) instrs = pure ()
            | otherwise = freeRegister a

outOfRange :: String -> Bool
outOfRange "" = False
outOfRange reg@(_:_) = all (`elem` ("1234567890" :: String)) (tail reg)
                    && (("s" `isPrefixOf` reg && read (tail reg) > 7) ||
                        ("t" `isPrefixOf` reg && read (tail reg) > 9))

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

