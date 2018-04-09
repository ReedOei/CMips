module Optimizer where

import Control.Monad
import Control.Monad.State

import Data.List ((\\), isPrefixOf)

import Compiler.Types
import MIPSLanguage

import Util

import System.IO.Unsafe

optimize :: [MIPSInstruction] -> State Environment [MIPSInstruction]
optimize = allocateRegisters >=> handleResSave

getUnusedRegisters :: [String] -> [String]
getUnusedRegisters regs = avail \\ regs
    where avail = map (("t" ++) . show) [0..9] ++ map (("s" ++) . show) [0..7]

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

                pure $ replaceOperand instr a reg
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

                    let newInstr = replaceOperand (Inst op a b c) saveReg reg

                    -- Keep going until there is no more stack access to handle.
                    handleResSave $ beforeLoad ++ startInstr ++ [newInstr] ++ endInstr ++ afterLoad

