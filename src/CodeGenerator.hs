module CodeGenerator where

import Data.Char (isAlpha)
import Data.List (intercalate)

import Compiler
import MIPSLanguage

printCode :: MIPSFile -> IO ()
printCode = putStrLn . generateFile

generateFile :: MIPSFile -> String
generateFile (MIPSFile _ instructions) = intercalate "\n\n" $ map generate instructions

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

generateInstruction :: MIPSInstruction -> String
generateInstruction Empty = ""
generateInstruction (Label labelName) = labelName ++ ":"
generateInstruction (Comment comment) = "# " ++ comment
generateInstruction (Inst funct rd rs rt) =
    case funct of
        OP_MOVE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs
        OP_LI -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs
        OP_LW -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_LB -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_SW -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_SB -> mnemonic funct ++ " $" ++ rd ++ ", " ++ rs ++ "($" ++ rt ++ ")"
        OP_J -> mnemonic funct ++ " " ++ rd
        OP_JR -> mnemonic funct ++ " $" ++ rd
        OP_JAL -> mnemonic funct ++ " " ++ rd
        OP_BNE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BEQ -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BGT -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BGE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BLT -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_BLE -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ rt
        OP_NOT -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs
        _ -> mnemonic funct ++ " $" ++ rd ++ ", $" ++ rs ++ ", " ++ regName rt

