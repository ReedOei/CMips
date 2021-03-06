{-# LANGUAGE TupleSections #-}

module Compiler.Compiler where

import Control.Lens ((^.), view, over)
import Control.Monad
import Control.Monad.State

import Data.Char (ord)
import Data.List (isInfixOf, isPrefixOf, find, findIndex, intersperse, (\\), nub)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)

import Text.ParserCombinators.Parsec (parse)

import Analysis.Analyzer

import Compiler.Resolver
import Compiler.Types
import Optimizer
import CLanguage
import MIPSLanguage
import MIPSParser
import Parser (loadFile)
import Types
import Util

import System.IO.Unsafe

isInclude (Preprocessor Include _) = True
isInclude _ = False

-- Do init and tail to get rid of the quotes around it.
getInclude (Preprocessor Include filename) = init $ tail filename

mergeFiles :: CFile -> CFile -> CFile
mergeFiles (CFile mainName mainElements) (CFile _ includedElements) = CFile mainName $ includedElements ++ mainElements

preprocessor :: CFile -> IO CFile
preprocessor mainFile@(CFile fileName elements) = do
    includedFiles <- mapM ((loadFile >=> preprocessor) . getInclude) $ filter isInclude elements
    pure $ foldl mergeFiles mainFile includedFiles

compile :: CFile -> IO (Either [Warning] MIPSFile)
compile file = compileWith defaultCompileOptions =<< preprocessor file

compileWith :: CompileOptions -> CFile -> IO (Either [Warning] MIPSFile)
compileWith opts file = mainCompile opts <$> preprocessor file

mainCompile :: CompileOptions -> CFile -> Either [Warning] MIPSFile
mainCompile opts file@(CFile fname initElements) =
    -- case analyze file of
    --     [] ->
            case env ^. warnings of
                [] -> Right $ MIPSFile fname sections $ filter (not . null) instructions
                warnings -> Left warnings
        -- warnings -> Left warnings
    where
        sections = [generateDataSection "data" d, generateDataSection "kdata" kd, MIPSSection "text" []]
        (Data d kd) = view dataSections env
        (instructions, env) = runState state $ newEnvironment opts $ CFile fname elements
        elements = fakeFunctions ++ initElements
        fakeFunctions = concatMap generateFuncDec initElements
        state = foldM go [] =<< mapM elaborateTypes elements
        go prev element = do
            newInstructions <- compileElement element
            modify resetLocal

            return $ prev ++ [newInstructions]

generateFuncDec :: CElement -> [CElement]
generateFuncDec (Inline retType funcName vars _) = [FuncDef retType funcName vars []]
generateFuncDec _ = []

generateDataSection :: String -> [DataElement] -> MIPSSection
generateDataSection sectionName elements =
    MIPSSection sectionName $ map (\(DataElement name dataType value) -> (name, dataType, value)) elements

saveStack :: Int -> [String] -> [MIPSInstruction]
saveStack _ [] = []
saveStack reserved registers = Inst OP_SUB "sp" "sp" (show (reserved + 4 * length registers)) : saveInstr ++ [Empty]
    where saveInstr = map (\(i, r) -> Inst OP_SW r (show (reserved + i * 4)) "sp") $ zip [0..] registers

restoreStack :: Int -> [String] -> [MIPSInstruction]
restoreStack _ [] = []
restoreStack reserved registers = restoreInstr ++ [Inst OP_ADD "sp" "sp" (show (reserved + 4 * length registers))]
    where restoreInstr = map (\(i, r) -> Inst OP_LW r (show (reserved + i * 4)) "sp") $ zip [0..] registers

compileElement :: CElement -> State Environment [MIPSInstruction]
compileElement func@(FuncDef t funcName args statements) =
    if null statements then
        pure []
    else do
        (label, funcEnd) <- funcLabel funcName

        setCurFunc funcName

        argInstr <- generateArgs args

        instr <- compileStatements statements

        let body = argInstr ++ instr ++ [Empty, Label funcEnd]

        optLevel <- view (compileOptions . optimizeLevel) <$> get
        finalInstr <-
            if optLevel > 0 then
                optimize body >>= allocate
            else
                allocate body

        let usedSRegs = nub $ filter (isRegType "s") $ concatMap getOperands finalInstr
        let saveRegs =
                case find isCall body of
                    -- We only need to save the return address if there's a jal in the body.
                    Nothing -> usedSRegs
                    Just _ -> "ra" : usedSRegs

        reserved <- stalloc "" 0 -- Requesting 0 more bytes will give us the top
        modify $ purgeRegTypeEnv "s"
        modify $ purgeRegTypeEnv "t"


        let asm = Label label :
                  Comment (prettyPrint func) :
                  saveStack reserved saveRegs ++
                  finalInstr ++
                  restoreStack reserved saveRegs ++
                  freeMemory ++
                  [Inst OP_JR "ra" "" ""]

        modify $ over compiled (Map.insert funcName asm)

        pure asm
    where
        freeMemory =
            case funcName of
                "main" -> [Inst OP_LI "v0" "10" "", Inst SYSCALL "" "" ""]
                _ -> []
compileElement func@(Inline t funcName args statements) = do
    (label, funcEnd) <- funcLabel funcName

    let asm = Label label :
              Comment (prettyPrint func) :
              map (\str -> transformLitAsm (Inst LIT_ASM str "" "")) statements

    modify $ over compiled (Map.insert funcName asm)

    pure asm

    where
        transformLitAsm i@(Inst LIT_ASM str "" "") =
            case parse mipsLine "" str of
                Left err -> error $ "Error while parsing line of literal mips code: " ++ show err ++ "\n" ++
                                    "MIPS Code: " ++ str
                Right instr -> instr
        transformLitAsm i = i

compileElement (StructDef structName _) = pure [Comment $ "struct " ++ structName]
compileElement (CommentElement (CComment str)) = pure [Comment str]

compileElement _ = pure []

compileStatements :: [CStatement] -> State Environment [MIPSInstruction]
compileStatements statements = do
    instr <- foldM go [] statements

    pure instr
    where
        go instructions statement = do
            newInstr <- compileStatement statement
            pure $ instructions ++ newInstr

compileCondition :: String -> String -> CExpression -> State Environment [MIPSInstruction]
compileCondition trueLabel falseLabel st@(CBinaryOp And a b) =  do
    aInstr <- compileCondition "" falseLabel a
    bInstr <- compileCondition "" falseLabel b
    aInstr <-
        case a of
            CBinaryOp Or _ _ -> compileCondition trueLabel falseLabel a
            CBinaryOp And _ _ -> filter (not . isJ) <$> compileCondition trueLabel falseLabel a
            _ -> compileCondition "" falseLabel a
    bInstr <-
        case b of
            CBinaryOp Or _ _ -> compileCondition trueLabel falseLabel b
            CBinaryOp And _ _ -> filter (not . isJ) <$> compileCondition trueLabel falseLabel b
            _ -> compileCondition "" falseLabel b

    pure $ aInstr ++ bInstr ++ [Inst OP_J trueLabel "" ""]

    where
        isJ (Inst OP_J _ _ _) = True
        isJ _ = False

compileCondition trueLabel falseLabel st@(CBinaryOp Or a b) = do
    aInstr <-
        case a of
            CBinaryOp Or _ _ -> filter (not . isJ) <$> compileCondition trueLabel falseLabel a
            CBinaryOp And _ _ -> compileCondition trueLabel falseLabel a
            _ -> compileCondition trueLabel "" a
    bInstr <-
        case b of
            CBinaryOp Or _ _ -> filter (not . isJ) <$> compileCondition trueLabel falseLabel b
            CBinaryOp And _ _ -> compileCondition trueLabel falseLabel b
            _ -> compileCondition trueLabel "" b

    pure $ aInstr ++ bInstr ++ [Inst OP_J falseLabel "" ""]

    where
        isJ (Inst OP_J _ _ _) = True
        isJ _ = False

compileCondition trueLabel falseLabel expr@(CBinaryOp op a b) = do
    (aReg, aInstr) <- compileExpression a
    (bReg, bInstr) <- compileExpression b

    aType <- resolveType a >>= elaborateType
    bType <- resolveType b >>= elaborateType

    if isFloating aType || isFloating bType then do
        aFloatingReg <- useNextRegister "result_float" "a_cond_float"
        bFloatingReg <- useNextRegister "result_float" "b_cond_float"

        instr <- compileFloatCondition trueLabel falseLabel op aFloatingReg bFloatingReg

        if isFloating aType && isFloating bType then
            pure $ aInstr ++ bInstr ++ [Inst OP_MTC1 aReg aFloatingReg "", Inst OP_MTC1 bReg bFloatingReg ""] ++ instr
        else if isFloating aType && isIntegral bType then
            pure $ aInstr ++ bInstr ++ [Inst OP_MTC1 aReg aFloatingReg "", Inst OP_MTC1 bReg bFloatingReg "", Inst OP_CVT_S_W bFloatingReg bFloatingReg ""] ++ instr
        else
            pure $ aInstr ++ bInstr ++ [Inst OP_MTC1 aReg aFloatingReg "", Inst OP_CVT_S_W aFloatingReg aFloatingReg "", Inst OP_MTC1 bReg bFloatingReg ""] ++ instr
    else let opposite = getBranchOpNeg op in do
        res <- if op `elem` [CEQ, CNE, CLT, CGT, CLTE, CGTE] then
                    if not $ null falseLabel then
                        pure $ aInstr ++ bInstr ++ [Inst opposite aReg bReg falseLabel]
                    else
                        pure $ aInstr ++ bInstr ++ [Inst (getBranchOp op) aReg bReg trueLabel]
                else
                    compileCondition trueLabel falseLabel (CBinaryOp CNE expr (LitInt 0))

        if countFunctionCalls expr > 0 then
            makeCallSafe res
        else
            pure res
compileCondition trueLabel falseLabel expr = compileCondition trueLabel falseLabel (CBinaryOp CNE expr (LitInt 0))

compileFloatCondition trueLabel falseLabel op aReg bReg = do
    let invert = not (op == CGT || op == CNE || op == CGTE)
    let comparison = case op of
                    CEQ -> [Inst OP_CEQS aReg bReg ""]
                    CNE -> [Inst OP_CEQS aReg bReg ""]
                    CLT -> [Inst OP_CLTS aReg bReg ""]
                    CLTE -> [Inst OP_CLES aReg bReg ""]
                    CGT -> [Inst OP_CLES aReg bReg ""]
                    CGTE -> [Inst OP_CLTS aReg bReg ""]
    let target = if not $ null falseLabel then falseLabel else trueLabel
    if invert then
        pure $ comparison ++ [Inst OP_BC1F target "" ""]
    else
        pure $ comparison ++ [Inst OP_BC1T target "" ""]

-- Returns the label to go to in order to skip this block.
handleIfStatement :: CStatement -> State Environment (String, [MIPSInstruction])
handleIfStatement st@(ElseBlock statements) = do
    labelEnd <- getNextLabel "else_end"
    instr <- compileStatements statements

    pure (labelEnd, Empty : Comment (prettyPrint st) : instr ++ [Label labelEnd])

handleIfStatement st@(IfStatement cond branches body) = do
    labelStart <- getNextLabel "if"
    labelBody <- getNextLabel "if_body"
    labelEnd <- getNextLabel "if_end"

    instr <- compileCondition labelBody labelEnd cond
    finalEnv <- get

    bodyInstr <- compileStatements body
    branchInstr <-
        case branches of
            Nothing -> pure [Label labelEnd]
            Just branch -> do
                (branchEnd, instrs) <- handleIfStatement branch
                pure $ [Inst OP_J branchEnd "" "", Label labelEnd] ++ instrs

    pure (labelEnd, [Empty, Comment (prettyPrint st), Label labelStart] ++ instr ++ [Label labelBody] ++ bodyInstr ++ branchInstr)

----------------------------------
-- Compile statements
----------------------------------
compileStatement :: CStatement -> State Environment [MIPSInstruction]
compileStatement (CComment str) = pure [Comment str]
compileStatement (Annotated _ (Just stmt)) = compileStatement stmt
compileStatement (Annotated _ Nothing) = pure []
compileStatement st@(VarDef (Var _ FunctionPointer{} varName) ini) = do
    reg <- useNextRegister "result_save" varName

    case ini of
        Just (VarRef name) -> do
            fInfo <- getFuncLabel name
            case fInfo of
                Just (funcLabel, _) -> pure [Empty, Comment (prettyPrint st), Inst OP_LA reg funcLabel ""]
        Just initializer -> do
            (source, instructions) <- compileExpression initializer
            pure $ Empty : Comment (prettyPrint st) : instructions ++ [Inst OP_MOVE reg source ""]
        Nothing -> pure [Empty, Comment (prettyPrint st)]

compileStatement (VarDef (Var annotations (NamedType name) varName) ini) = compileStatement (VarDef (Var annotations (Type Value (NamedType name)) varName) ini)
compileStatement st@(VarDef (Var _ t@(Type varKind typeName) varName) ini) = do
    reg <- useNextRegister "result_save" varName

    case ini of
        -- If there's no initializer, all we need to do is take note of the fact that this variable exists
        Nothing -> pure [Empty, Comment (prettyPrint st)]
        Just (LitInt n) -> pure [Empty, Comment (prettyPrint st), Inst OP_LI reg (show n) ""]
        Just initializer -> do
            (source, instructions) <- compileExpression initializer

            varType <- elaborateType t
            initType <- resolveType initializer >>= elaborateType

            convertInstr <- convert (source, initType) (reg, varType)

            pure $ Empty : Comment (prettyPrint st) : instructions ++ convertInstr


compileStatement ifStatement@IfStatement{} = do
    (_, instr) <- handleIfStatement ifStatement
    pure instr

compileStatement st@(WhileStatement cond body) = do
    labelStart <- getNextLabel "while"
    labelBody <- getNextLabel "while_body"
    labelEnd <- getNextLabel "while_end"

    instr <- compileCondition labelBody labelEnd cond
    bodyInstr <- compileStatements body

    pure $ [Empty, Comment (prettyPrint st), Label labelStart] ++ instr ++ [Label labelBody] ++ bodyInstr ++ [Inst OP_J labelStart "" "", Label labelEnd]

compileStatement st@(ForStatement ini cond step body) = do
    instrIni <- compileStatement ini
    instr <- compileStatement $ WhileStatement cond (body ++ [step])

    pure $ Empty : Comment (prettyPrint st) : instrIni ++ instr

compileStatement st@(Return Nothing) = do
    fInfo <- getFuncLabel =<< getCurFunc

    let funcEnd = case fInfo of
                    Nothing -> error "Unknown current function"
                    Just (_, endLabel) -> endLabel

    pure [Empty, Comment (prettyPrint st), Inst OP_J funcEnd "" ""]

compileStatement st@(Return (Just expr)) = do
    (source, instr) <- compileExpression expr

    fInfo <- getFuncLabel =<< getCurFunc

    let funcEnd = case fInfo of
                    Nothing -> error "Unknown current function"
                    Just (_, endLabel) -> endLabel

    callRes <- resolveFuncCall =<< getCurFunc
    case callRes of
        Just (FuncDef t _ _ _) -> do
            retType <- elaborateType t
            exprType <- resolveType expr >>= elaborateType

            convertInstr <- convert (source, exprType) ("v0", retType)

            pure $ Empty : Comment (prettyPrint st) : instr ++ convertInstr ++ [Inst OP_J funcEnd "" ""]
        _ -> error "Could not find function call"

compileStatement (Assign (Just op) lhs rhs) = compileStatement $ Assign Nothing lhs (CBinaryOp op lhs rhs)
compileStatement st@(Assign Nothing lhs rhs) = do
    lhsType <- resolveType lhs >>= elaborateType
    rhsType <- resolveType rhs >>= elaborateType

    (source, instr) <- compileExpression rhs

    (reg, accessInstr, postAccessInstr) <- do
            (reg, instr) <- compileExpression lhs

            if not $ null instr then
                case last instr of
                    Inst OP_LW target offset loadSource -> do
                        tempReg <- useNextRegister "result_save" "temp_load"
                        assign <- convert (source, rhsType) (tempReg, lhsType)
                        pure (tempReg, init instr, assign ++ [Inst OP_SW tempReg offset loadSource])
                    Inst OP_LB target offset loadSource -> do
                        tempReg <- useNextRegister "result_save" "temp_load"
                        assign <- convert (source, rhsType) (tempReg, lhsType)
                        pure (tempReg, init instr, assign ++ [Inst OP_SB tempReg offset loadSource])
                    inst -> error $ "Unexpected instruction for assign expression: " ++ show st ++ " " ++ show inst
            else do
                assign <- convert (source, rhsType) (reg, lhsType)
                pure (reg, [], assign)

    pure $ Empty : Comment (prettyPrint st) : accessInstr ++ instr ++ postAccessInstr

compileStatement st@(ExprStatement expr) = do
    (_, instr) <- compileExpression expr

    pure $ Empty : Comment (prettyPrint st) : instr

---------------------------------------------
-- Compile Expressions
---------------------------------------------
countFunctionCalls :: CExpression -> Int
countFunctionCalls (FuncCall _ exprs) = 1 + sum (map countFunctionCalls exprs)
countFunctionCalls (MemberAccess a b) = countFunctionCalls a + countFunctionCalls b
countFunctionCalls (CPrefix _ expr) = countFunctionCalls expr
countFunctionCalls (CPostfix _ expr) = countFunctionCalls expr
countFunctionCalls (CArrayAccess a b) = countFunctionCalls a + countFunctionCalls b
countFunctionCalls (CBinaryOp _ a b) = countFunctionCalls a + countFunctionCalls b
countFunctionCalls _ = 0

getVarRefs :: CExpression -> [String]
getVarRefs (VarRef x) = [x]
getVarRefs (FuncCall fname exprs) = fname : concatMap getVarRefs exprs -- fname could be a var ref, if it is a function pointer variable.
getVarRefs (MemberAccess a b) = getVarRefs a -- Only refs in the front half will count, because the ones in the second half will just be members
getVarRefs (CPrefix _ expr) = getVarRefs expr
getVarRefs (CPostfix _ expr) = getVarRefs expr
getVarRefs (CArrayAccess a b) = getVarRefs a ++ getVarRefs b
getVarRefs (CBinaryOp _ a b) = getVarRefs a ++ getVarRefs b
getVarRefs _ = []

getReg :: String -> State Environment String
getReg r
    | isRegType "result_temp" r = do
        exists <- registerNameExists r

        if exists then
            getRegister r
        else
            useNextRegister "result_save" $ "save_" ++ r
    | otherwise = pure r

useStack :: MIPSInstruction -> State Environment MIPSInstruction
useStack (Inst op a b c) = Inst op <$> getReg a <*> getReg b <*> getReg c
useStack i = pure i

makeCallSafe :: [MIPSInstruction] -> State Environment [MIPSInstruction]
makeCallSafe = mapM useStack

-- Compile expressions, but check if there is more than one function call in the expression.
-- If so, we will save all temp registers on the stack to be safe.
compileExpression :: CExpression -> State Environment (String, [MIPSInstruction])
compileExpression expr = do
    (reg, instr) <- compileExpressionTemp expr

    if countFunctionCalls expr > 0 then
        case expr of
            -- If there's only one function call and this expression is it, we don't need to waste time with saving stuff on the stack.
            FuncCall{} | countFunctionCalls expr == 1 -> pure (reg, instr)
            _ -> (,) <$> getReg reg <*> mapM useStack instr
    else
        pure (reg, instr)

compileExpressionTemp :: CExpression -> State Environment (String, [MIPSInstruction])
compileExpressionTemp (VarRef "INT_MIN") = do
    reg <- useNextRegister "result_save" "INT_MIN_VAL"
    pure (reg, [Inst OP_LI reg "-2147483648" ""])
compileExpressionTemp (VarRef "INT_MAX") = do
    reg <- useNextRegister "result_save" "INT_MAX_VAL"
    pure (reg, [Inst OP_LI reg "2147483647" ""])
compileExpressionTemp (VarRef varName) = do
    exists <- registerNameExists varName

    if exists then
        (,[]) <$> getRegister varName
    else do
        fInfo <- getFuncLabel varName

        case fInfo of
            Just (funcLabel, _) -> do
                reg <- useNextRegister "result_save" $ varName ++ "_" ++ funcLabel

                pure (reg, [Inst OP_LA reg funcLabel ""])

            Nothing -> (,[]) <$> getRegister varName

-- Special case for 0 because $0 always contains 0 -- this can save us an OP_LI
compileExpressionTemp (LitInt 0) = pure ("0", [])
compileExpressionTemp (LitInt i) = do
    reg <- useNextRegister "result_save" $ show i
    pure (reg, [Inst OP_LI reg (show i) ""])

compileExpressionTemp (LitChar c) = do
    reg <- useNextRegister "result_save" $ show $ ord c
    pure (reg, [Inst OP_LI reg (show (ord c)) ""])

compileExpressionTemp (LitString s) = do
    name <- saveData "asciiz" s
    reg <- useNextRegister "result_save" name
    pure (reg, [Inst OP_LA reg name ""])

compileExpressionTemp (LitFloat f) = do
    floatReg <- useNextRegister "result_float" "load_float_constant"
    reg <- useNextRegister "result_save" $ show f

    pure (reg, [Inst OP_LIS floatReg (show f) "", Inst OP_MFC1 reg floatReg ""])


compileExpressionTemp NULL =  pure ("0", [])

compileExpressionTemp (CArrayAccess accessExpr expr) = do
    (source, instr) <- compileExpressionTemp expr
    (access, accessInstr) <- compileExpressionTemp accessExpr

    dest <- useNextRegister "result_save" $ prettyPrint accessExpr ++ "_access"
    varType <- resolveType (CPrefix Dereference accessExpr) >>= elaborateType

    doAccess <- case accessExpr of
                        MemberAccess e (VarRef name) -> do
                            pure $ init accessInstr
                            t <- resolveType accessExpr >>= elaborateType

                            case t of
                                Array _ _ -> pure $ init accessInstr
                                _ -> pure accessInstr
                        _ -> pure accessInstr

    structOffset <- case accessExpr of
                        MemberAccess e (VarRef name) -> do
                            t <- resolveType accessExpr >>= elaborateType

                            case t of
                                Array _ _ ->
                                    case last accessInstr of
                                        Inst OP_LW loadDest offset loadSource -> pure $ init accessInstr ++ [Inst OP_ADD loadDest loadSource offset]
                                        Inst OP_LB loadDest offset loadSource -> pure $ init accessInstr ++ [Inst OP_ADD loadDest loadSource offset]
                                        inst -> error $ "Unexpected instruction after array access(" ++ show accessExpr ++ "): " ++ show inst
                                _ -> pure []
                        _ -> pure []

    case sizeof varType of
        1 -> pure (dest, instr ++
                         doAccess ++
                         structOffset ++
                         [Inst OP_ADD dest source access, -- Calculate address to load.
                          Inst OP_LB dest "0" dest]) -- Load memory, using lb instead of lw.
        size -> pure (dest, instr ++
                            doAccess ++
                            [Inst OP_MUL dest source $ show $ sizeof varType] ++
                            structOffset ++
                            [Inst OP_ADD dest dest access, -- Calculate address to load.
                             Inst OP_LW dest "0" dest]) -- Load memory

compileExpressionTemp (CBinaryOp op a b) = do
    (aReg, aInstr) <- compileExpressionTemp a
    (bReg, bInstr) <- compileExpressionTemp b
    reg <- useNextRegister "result_save" "temp"

    case op of
        -- In the case of CEQ, we compare for equality via:
        -- li reg, 1
        -- beq a, b, end
        -- li reg, 0 # Will be skipped if the two are equal
        -- end:
        CEQ -> do
            endEqualityTest <- getNextLabel "end_eq_test"
            pure (reg, aInstr ++ bInstr ++ [Inst OP_LI reg "1" "", Inst OP_BEQ aReg bReg endEqualityTest, Inst OP_LI reg "0" "", Label endEqualityTest])
        _ -> do
            aType <- resolveType a >>= elaborateType
            bType <- resolveType b >>= elaborateType

            case getPriorityType aType bType of
                NamedType "float" -> do
                    aFloatReg <- useNextRegister "result_float" "float_reg_a"
                    bFloatReg <- useNextRegister "result_float" "float_reg_b"
                    resultFloatReg <- useNextRegister "result_float" "float_reg_result"

                    let aConv =
                            case aType of
                                NamedType "float" -> [Inst OP_MTC1 aReg aFloatReg ""]
                                _ -> [Inst OP_MTC1 aReg aFloatReg "", Inst OP_CVT_S_W aFloatReg aFloatReg ""]
                    let bConv =
                            case bType of
                                NamedType "float" -> [Inst OP_MTC1 bReg bFloatReg ""]
                                _ -> [Inst OP_MTC1 bReg bFloatReg "", Inst OP_CVT_S_W bFloatReg bFloatReg ""]

                    pure (reg, aInstr ++ bInstr ++ aConv ++ bConv ++
                               [Inst (opFindFloat op) resultFloatReg aFloatReg bFloatReg,
                                Inst OP_MFC1 reg resultFloatReg ""])
                _ -> pure (reg, aInstr ++ bInstr ++ [Inst (opFind op) reg aReg bReg])

compileExpressionTemp (CPrefix PreIncrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_ADD source source "1"])

compileExpressionTemp (CPrefix PreDecrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_SUB source source "1"])

compileExpressionTemp (CPrefix Dereference a) = do
    (source, instr) <- compileExpressionTemp a
    dest <- useNextRegister "result_save" $ "temp_deref_" ++ show a
    pure (dest, instr ++ [Inst OP_LW dest "0" source])

compileExpressionTemp expr@(CPrefix AddressOf a) = do
    (temp, instr) <- compileExpressionTemp a

    if not $ null instr then
        case last instr of
            Inst OP_LW dest offset sourceReg -> pure (sourceReg, init instr ++ [Inst OP_ADD sourceReg sourceReg offset])
            Inst OP_LB dest offset sourceReg -> pure (sourceReg, init instr ++ [Inst OP_ADD sourceReg sourceReg offset])
            _ -> error $ "Trying to get the address of a non-memory operation: " ++ show expr
    else
        error $ "Trying to get the address of a non-memory operation: " ++ show expr

compileExpressionTemp (CPrefix PreNot a) = do
    (source, instr) <- compileExpressionTemp a
    endNot <- getNextLabel "end_not"
    reg <- useNextRegister "result_save" "temp"

    pure (reg, instr ++ [Inst OP_LI reg "1" "", Inst OP_BEQ source "0" endNot, Inst OP_LI reg "0" "", Label endNot])

compileExpressionTemp (CPostfix PostIncrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_ADD source source "1"])

compileExpressionTemp (CPostfix PostDecrement a) = do
    (source, instr) <- compileExpressionTemp a
    pure (source, instr ++ [Inst OP_SUB source source "1"])

compileExpressionTemp accessExpr@(MemberAccess expr (VarRef name)) = do
    t <- resolveType expr >>= elaborateType

    memberType <- resolveType accessExpr >>= elaborateType

    let accessOp = if sizeof memberType == 1 then OP_LB else OP_LW

    (source, instr) <- compileExpressionTemp expr
    n <- getStructOffset expr name

    if not $ null instr then
        case last instr of
            Inst OP_LW a _ b -> pure (a, init instr ++ [Inst accessOp a (show n) b])
            _ -> do
                dest <- useNextRegister "result_save" $ "access_" ++ name
                pure (dest, instr ++ [Inst accessOp dest (show n) source])
    else do
        dest <- useNextRegister "result_save" $ "access_" ++ name
        pure (dest, instr ++ [Inst accessOp dest (show n) source])

-- t0 is just a dummy register because we should never use the value that comes from calling printf.
compileExpressionTemp (FuncCall "printf" args) = ("t0",) <$> compilePrintf args
compileExpressionTemp (FuncCall "malloc" [arg]) = compileMalloc arg
compileExpressionTemp (FuncCall "sizeof" [arg]) = compileSizeof arg
-- compileExpressionTemp (FuncCall "free" [arg]) = compileFree arg -- TODO: Figure this out.

compileExpressionTemp (FuncCall funcName args) = do
    fType <- resolveFuncType funcName

    let argTypes = case fType of
                        Just (_, as) -> as
                        Nothing -> error $ "Undefined reference to: '" ++ funcName ++ "', can't get argument types!"

    let go (curRegs, curInstr) expr = do
            (reg, newInstr) <- compileExpressionTemp expr
            pure (curRegs ++ [reg], curInstr ++ newInstr)

    (regs, instr) <- foldM go ([], []) args

    argLoading <- concat <$> zipWithM doArgConvert (zip [0..] argTypes) (zip regs args)

    res <- getFuncLabel funcName
    retVal <- useNextRegister "result_save" "func_call_return_val"

    jumpOp <- case res of
                    -- If we don't find it, see if we have a function pointer for it.
                    Nothing -> do
                        r <- getRegister funcName
                        pure $ Inst OP_JALR r "" ""
                    Just (label, _) -> pure $ Inst OP_JAL label "" ""
    pure (retVal,
             instr ++ argLoading ++ [jumpOp] ++
             [Inst OP_MOVE retVal "v0" ""]) -- Make sure to save func call result.

    where
        doArgConvert (i, argType) (source, sourceExpr) = do
            sourceType <- resolveType sourceExpr >>= elaborateType

            let argReg = "a" ++ show i
            convert (source, sourceType) (argReg, argType)

compileExpressionTemp i = error $ "Not implemented: " ++ show i

compileSizeof :: CExpression -> State Environment (String, [MIPSInstruction])
compileSizeof expr = do
    t <- resolveType expr >>= elaborateType
    reg <- useNextRegister "result_save" $ "sizeof(" ++ show expr ++ ")"

    pure (reg, [Inst OP_LI reg (show (sizeof t)) ""])

compileMalloc :: CExpression -> State Environment (String, [MIPSInstruction])
compileMalloc expr = do
    (reg, instr) <- compileExpressionTemp expr

    pure ("v0", instr ++ [Inst OP_LI "v0" "9" "", Inst OP_MOVE "a0" reg "", Inst SYSCALL "" "" ""])

compilePrintf :: [CExpression] -> State Environment [MIPSInstruction]
compilePrintf (LitString formatStr:args) = do
    -- Split up the format string and find the things we need to insert (only strings and integers for now).
    let elements = concatMap (intersperse "%f" . splitOn "%f") $ concatMap (intersperse "%d" . splitOn "%d") $ intersperse "%s" $ splitOn "%s" formatStr

    (_, instr) <- foldM go (args, []) elements
    pure instr

    where
        go x "" = pure x

        go (a:as, curInstr) "%f" = do
            (reg, instr) <- compileExpressionTemp a

            -- 12 MUST BE USED.
            pure (as, curInstr ++ instr ++ [Inst OP_MTC1 reg "f12" "", Inst OP_LI "v0" "2" "", Inst SYSCALL "" "" ""]) -- 2 is print float.

        go (a:as, curInstr) "%s" = do
            (reg, instr) <- compileExpressionTemp a
            t <- resolveType a >>= elaborateType
            case t of
                Type Pointer (NamedType "char") ->
                    pure (as, curInstr ++ instr ++ [Inst OP_MOVE "a0" reg "", Inst OP_LI "v0" "4" "", Inst SYSCALL "" "" ""]) -- 4 is print string
                _ -> error $ "Type is not char*, cannot print: '" ++ show t ++ "'"

        go (a:as, curInstr) "%d" = do
            (reg, instr) <- compileExpressionTemp a
            pure (as, curInstr ++ instr ++ [Inst OP_MOVE "a0" reg "", Inst OP_LI "v0" "1" "", Inst SYSCALL "" "" ""]) -- 1 is print int.

        go (as, curInstr) str = do
            name <- saveData "asciiz" str
            reg <- useNextRegister "result_save" name
            pure (as, curInstr ++ [Inst OP_LA "a0" name "", Inst OP_LI "v0" "4" "", Inst SYSCALL "" "" ""]) -- 4 is print string

