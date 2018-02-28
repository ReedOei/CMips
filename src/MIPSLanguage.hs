module MIPSLanguage where

data MIPSFile = MIPSFile String [MIPSInstruction]
    deriving Show

data MIPSInstruction = Inst MIPSOp String String String
                     | Label String
    deriving Show

data MIPSOp = OP_ADD
            | OP_MOVE
            | OP_LI
            | OP_MUL
            | OP_LW
    deriving Show

