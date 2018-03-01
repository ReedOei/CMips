module MIPSLanguage where

import CLanguage

data MIPSFile = MIPSFile String [[MIPSInstruction]] -- Each list contains one function def.
    deriving Show

data MIPSInstruction = Inst MIPSOp String String String
                     | Label String
                     | Comment String
                     | Empty
    deriving Show

data MIPSOp = OP_ADD
            | OP_MOVE
            | OP_LI
            | OP_MUL
            | OP_LW
            | OP_SW
            | OP_XOR
            | OP_DIV
            | OP_SUB
            | OP_AND
            | OP_OR
            | OP_BNE
            | OP_BEQ
            | OP_BGT
            | OP_BGE
            | OP_BLT
            | OP_BLE
            | OP_J
            | OP_JR
            | OP_JAL
            | OP_SLL
            | OP_SRL
            | OP_REM
    deriving (Show, Eq)

mnemonic :: MIPSOp -> String
mnemonic OP_MOVE = "move"
mnemonic OP_REM = "rem"
mnemonic OP_LI = "li"
mnemonic OP_ADD = "add"
mnemonic OP_MUL = "mul"
mnemonic OP_SUB = "sub"
mnemonic OP_LW = "lw"
mnemonic OP_SW = "sw"
mnemonic OP_OR = "or"
mnemonic OP_AND = "and"
mnemonic OP_XOR = "xor"
mnemonic OP_DIV = "div"
mnemonic OP_BNE = "bne"
mnemonic OP_BGT = "bgt"
mnemonic OP_BLE = "ble"
mnemonic OP_BLT = "blt"
mnemonic OP_BGE = "bge"
mnemonic OP_BEQ = "beq"
mnemonic OP_J = "j"
mnemonic OP_JR = "jr"
mnemonic OP_JAL = "jal"
mnemonic OP_SLL = "sll"
mnemonic OP_SRL = "srl"

opFind :: BinaryOp -> MIPSOp
opFind Add = OP_ADD
opFind Mult = OP_MUL
opFind Xor = OP_XOR
opFind OrBit = OP_OR
opFind AndBit = OP_AND
opFind Minus = OP_SUB
opFind Div = OP_DIV
opFind ShiftLeft = OP_SLL
opFind ShiftRight = OP_SRL
opFind Mod = OP_REM
opFind And = OP_AND
opFind Or = OP_OR
opFind CNE = OP_SUB -- If they are the same, we will get 0, which is false.

getBranchOpNeg :: BinaryOp -> MIPSOp
getBranchOpNeg CEQ = OP_BNE
getBranchOpNeg CLTE = OP_BGT
getBranchOpNeg CLT = OP_BGE
getBranchOpNeg CGT = OP_BLE
getBranchOpNeg CGTE = OP_BLT
getBranchOpNeg CNE = OP_BEQ

isJAL (Inst OP_JAL _ _ _) = True
isJAL _ = False

