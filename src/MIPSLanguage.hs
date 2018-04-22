module MIPSLanguage where

import Data.Bits

import CLanguage

data MIPSFile = MIPSFile String [MIPSSection] [[MIPSInstruction]] -- Each list contains one function def.
    deriving Show

data MIPSSection = MIPSSection String [(String, String, String)]
    deriving Show

data MIPSInstruction = Inst MIPSOp String String String
                     | Label String
                     | Comment String
                     | Empty
    deriving (Eq, Show)

data MIPSOp = OP_ADD
            | OP_MOVE
            | OP_LI
            | OP_LA
            | OP_MUL
            | OP_LW
            | OP_SW
            | OP_LB
            | OP_SB
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
            | OP_JALR
            | OP_SLL
            | OP_SRL
            | OP_REM
            | OP_NOT
            | SYSCALL
            | LIT_ASM -- Used for inlining assembly.
    deriving (Show, Eq)

isLabel (Label _) = True
isLabel _ = False

mnemonic :: MIPSOp -> String
mnemonic OP_MOVE = "move"
mnemonic OP_REM = "rem"
mnemonic OP_LI = "li"
mnemonic OP_LA = "la"
mnemonic OP_ADD = "add"
mnemonic OP_MUL = "mul"
mnemonic OP_SUB = "sub"
mnemonic OP_LW = "lw"
mnemonic OP_SW = "sw"
mnemonic OP_LB = "lb"
mnemonic OP_SB = "sb"
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
mnemonic OP_JALR = "jalr"
mnemonic OP_SLL = "sll"
mnemonic OP_SRL = "srl"
mnemonic OP_NOT = "not"
mnemonic SYSCALL = "syscall"

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
opFind op = error $ "Unknown operation: '" ++ show op ++ "'"

getBranchOpNeg :: BinaryOp -> MIPSOp
getBranchOpNeg CEQ = OP_BNE
getBranchOpNeg CLTE = OP_BGT
getBranchOpNeg CLT = OP_BGE
getBranchOpNeg CGT = OP_BLE
getBranchOpNeg CGTE = OP_BLT
getBranchOpNeg CNE = OP_BEQ

getBranchOp :: BinaryOp -> MIPSOp
getBranchOp CEQ = OP_BEQ
getBranchOp CLTE = OP_BLE
getBranchOp CLT = OP_BLT
getBranchOp CGT = OP_BGT
getBranchOp CGTE = OP_BGE
getBranchOp CNE = OP_BNE

isCall (Inst OP_JAL _ _ _) = True
isCall (Inst OP_JALR _ _ _) = True
isCall (Inst SYSCALL _ _ _) = True
isCall _ = False

hasOperand :: (String -> Bool) -> MIPSInstruction -> Bool
hasOperand f (Inst _ a b c) = f a || f b || f c
hasOperand _ _ = False

getOperands :: MIPSInstruction -> [String]
getOperands (Inst _ a b c) = [a,b,c]
getOperands _ = []

setOperands :: MIPSInstruction -> [String] -> MIPSInstruction
setOperands (Inst op _ _ _) [a,b,c] = Inst op a b c
setOperands i _ = i

replaceOperand :: String -> String -> MIPSInstruction -> MIPSInstruction
replaceOperand search rep instr = setOperands instr $ map (\a -> if a == search then rep else a) $ getOperands instr

branchTarget :: MIPSInstruction -> Maybe String
branchTarget (Inst OP_J target _ _) = Just target
branchTarget (Inst OP_JAL target _ _) = Just target
branchTarget (Inst OP_BNE _ _ target) = Just target
branchTarget (Inst OP_BEQ _ _ target) = Just target
branchTarget (Inst OP_BGT _ _ target) = Just target
branchTarget (Inst OP_BGE _ _ target) = Just target
branchTarget (Inst OP_BLT _ _ target) = Just target
branchTarget (Inst OP_BLE _ _ target) = Just target
branchTarget _ = Nothing

isArith :: MIPSInstruction -> Bool
isArith (Inst OP_ADD _ _ _) = True
isArith (Inst OP_MUL _ _ _) = True
isArith (Inst OP_XOR _ _ _) = True
isArith (Inst OP_DIV _ _ _) = True
isArith (Inst OP_SUB _ _ _) = True
isArith (Inst OP_AND _ _ _) = True
isArith (Inst OP_OR _ _ _) = True
isArith _ = False

commutes OP_MUL = True
commutes OP_ADD = True
commutes OP_AND = True
commutes OP_OR = True
commutes OP_XOR = True
commutes _ = False

compute :: MIPSOp -> Integer -> Integer -> Integer
compute OP_ADD = (+)
compute OP_MUL = (*)
compute OP_XOR = xor
compute OP_DIV = div
compute OP_SUB = (-)
compute OP_AND = (.&.)
compute OP_OR = (.|.)

instResult :: MIPSInstruction -> Maybe String
instResult (Inst OP_ADD a _ _) = Just a
instResult (Inst OP_MOVE a _ _) = Just a
instResult (Inst OP_MUL a _ _) = Just a
instResult (Inst OP_LW a _ _) = Just a
instResult (Inst OP_LB a _ _) = Just a
instResult (Inst OP_XOR a _ _) = Just a
instResult (Inst OP_DIV a _ _) = Just a
instResult (Inst OP_SUB a _ _) = Just a
instResult (Inst OP_AND a _ _) = Just a
instResult (Inst OP_OR a _ _) = Just a
instResult (Inst OP_SLL a _ _) = Just a
instResult (Inst OP_SRL a _ _) = Just a
instResult (Inst OP_REM a _ _) = Just a
instResult (Inst OP_NOT a _ _) = Just a
instResult (Inst OP_LI a _ _) = Just a
instResult (Inst OP_LA a _ _) = Just a
instResult _ = Nothing

instUses :: MIPSInstruction -> [String]
instUses (Inst OP_MOVE _ b _) = [b]
instUses (Inst OP_ADD _ b c) = [b,c]
instUses (Inst OP_MUL _ b c) = [b,c]
instUses (Inst OP_LW a _ c) = [c]
instUses (Inst OP_SW a _ c) = [a,c]
instUses (Inst OP_LB a _ c) = [c]
instUses (Inst OP_SB a _ c) = [a,c]
instUses (Inst OP_XOR _ b c) = [b,c]
instUses (Inst OP_DIV _ b c) = [b,c]
instUses (Inst OP_SUB _ b c) = [b,c]
instUses (Inst OP_AND _ b c) = [b,c]
instUses (Inst OP_OR _ b c) = [b,c]
instUses (Inst OP_BNE a b _) = [a,b]
instUses (Inst OP_BEQ a b _) = [a,b]
instUses (Inst OP_BGT a b _) = [a,b]
instUses (Inst OP_BGE a b _) = [a,b]
instUses (Inst OP_BLT a b _) = [a,b]
instUses (Inst OP_BLE a b _) = [a,b]
instUses (Inst OP_JR a _ _) = [a]
instUses (Inst OP_JALR a _ _) = [a]
instUses (Inst OP_SLL _ b c) = [b,c]
instUses (Inst OP_SRL _ b c) = [b,c]
instUses (Inst OP_REM _ b c) = [b,c]
instUses (Inst OP_NOT _ b _) = [b]
instUses _ = []

