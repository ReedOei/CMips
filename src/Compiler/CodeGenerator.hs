module Compiler.CodeGenerator where

import Data.Char (isAlpha)
import Data.List (intercalate)

import Compiler.Compiler
import MIPSLanguage

printCode :: MIPSFile -> IO ()
printCode = putStrLn . generateFile

generateFile :: MIPSFile -> String
generateFile (MIPSFile _ sections instructions) =
    intercalate "\n\n" (filter (not . null) (map generateSection sections)) ++ "\n" ++
    intercalate "\n\n" (map generate instructions)

generateSection :: MIPSSection -> String
generateSection (MIPSSection "text" _) = ".text"
generateSection (MIPSSection _ []) = ""
generateSection (MIPSSection name d) =
    "." ++ name ++ "\n" ++
    intercalate "\n" (map generateData d)

generateData :: (String, String, String) -> String
generateData (name, dataType, dataVal) =
    case dataType of
        "asciiz" -> name ++ ": " ++ "." ++ dataType ++ " " ++ show dataVal
        _ -> name ++ ": " ++ "." ++ dataType ++ " " ++ dataVal

generate :: [MIPSInstruction] -> String
generate [] = ""
generate (inst:instructions) =
    generateInstruction inst ++ "\n" ++
    intercalate "\n" (map (("    " ++) . generateInstruction) instructions)

regName :: String -> String
regName "" = ""
regName reg@(c:_)
    | isAlpha c = "$" ++ reg
    | otherwise = reg

twoOp :: MIPSInstruction -> String
twoOp (Inst op a b _) = mnemonic op ++ " $" ++ a ++ ", $" ++ b

labelOp :: MIPSInstruction -> String
labelOp (Inst op a _ _) = mnemonic op ++ " " ++ a

memoryOp :: MIPSInstruction -> String
memoryOp (Inst op a b c) = mnemonic op ++ " $" ++ a ++ ", " ++ b ++ "($" ++ c ++ ")"

generateInstruction :: MIPSInstruction -> String
generateInstruction Empty = ""
generateInstruction (Label labelName) = labelName ++ ":"
generateInstruction (Comment comment) = "# " ++ comment
generateInstruction instr@(Inst funct rd rs rt) =
    case funct of
        SYSCALL -> "syscall"
        LIT_ASM -> rd
        OP_MOVE -> twoOp instr
        OP_LI -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs
        OP_LA -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs
        OP_LW -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_LB -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_SW -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_SB -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_J -> labelOp instr
        OP_JAL -> labelOp instr
        OP_JR -> mnemonic funct ++ " $" ++ rd
        OP_JALR -> mnemonic funct ++ " $" ++ rd
        OP_BNE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BEQ -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BGT -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BGE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BLT -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BLE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_NOT -> twoOp instr
        OP_MOVS -> twoOp instr
        OP_MTC1 -> twoOp instr
        OP_MFC1 -> twoOp instr
        OP_CVT_W_S -> twoOp instr
        OP_CVT_S_W -> twoOp instr
        OP_CEQS -> twoOp instr
        OP_CLES -> twoOp instr
        OP_CLTS -> twoOp instr
        OP_BC1F -> labelOp instr
        OP_BC1T -> labelOp instr
        OP_SS -> memoryOp instr
        OP_LS -> memoryOp instr
        OP_LIS -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs
        _ -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ regName rt

