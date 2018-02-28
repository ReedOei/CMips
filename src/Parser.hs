{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where
    -- (
    --     loadFile, fileParser,
    --     CFile,
    --     CElement, Var, Type, PreKind, VarKind
    -- ) where

import Control.Arrow
import Control.Monad

import Data.Maybe (isJust, maybe)

import Text.ParserCombinators.Parsec

import System.IO.Unsafe

import CLanguage
import Util (findSplit, maybePred)

loadFile :: String -> IO CFile
loadFile filename = do
    contents <- filter (/= '\r') <$> readFile filename

    case parse (fileParser filename) "Error: " contents of
      Left err -> error $ show err
      Right file -> return file

fileParser :: String -> CharParser st CFile
fileParser filename = CFile filename <$> cElementList

cElementList :: CharParser st [CElement]
cElementList = filter (/= MiscElement) <$> sepEndBy cElement (many1 (char '\n'))
-- cElementList = sepEndBy cElement (char '\n')

cElement :: CharParser st CElement
cElement = try preprocessorParser <|>
             try functionParser <|>
             try commentParser <|>
             pure MiscElement

cIdentifier :: CharParser st String
cIdentifier = do
    first <- letter
    rest <- many (alphaNum <|> oneOf "_") -- Allow : for scope resolution and ~ for destructors

    pure $ first:rest

commentParser :: CharParser st CElement
commentParser = do
    wsSkip
    string "//"
    many (noneOf "\n")
    return MiscElement

preprocessorParser :: CharParser st CElement
preprocessorParser = do
    char '#'

    preKindStr <- choice $ map try [string "include", string "ifndef", string "define", string "endif", many (noneOf " ")]


    let preKind = case preKindStr of
                    "include" -> Include
                    "ifndef" -> IfNDef
                    "define" -> Define
                    "endif" -> EndIf
                    _ -> MiscPreKind

    wsSkip

    val <- many (noneOf "\n")

    return $ Preprocessor preKind val

isConst :: CharParser st Bool
isConst = do
    wsSkip
    constM <- optionMaybe $ string "const"
    wsSkip

    return $ isJust constM

functionParser :: CharParser st CElement
functionParser = do
    returnType <- typeParser
    wsSkip

    funcName <- cIdentifier

    arguments <- between (char '(') (char ')') $ sepBy (try varParser) (char ',')

    wsSkip

    -- const <- isConst
    wsSkip
    next <- lookAhead (choice [char ';', char '{'])

    if next == ';' then do
        char ';'

        pure $ FuncDef returnType funcName arguments False []
    else do
        body <- block

        pure $ FuncDef returnType funcName arguments False body

typeParser :: CharParser st Type
typeParser = do
    -- const <- isConst

    wsSkip
    typeName <- do
        t <- cIdentifier
        notFollowedBy (char '(')
        return t
    wsSkip

    wsSkip
    varKindStr <- optionMaybe $ char '*'
    wsSkip

    let varKind = case varKindStr of
                      Just '*' -> Pointer
                      Nothing -> Value

    return $ Type False varKind typeName

statementParser :: CharParser st CStatement
statementParser = do
    wsSkip
    val <- try returnParser <|>
           try varDefParser <|>
           try whileStatementParser <|>
           try ifStatementParser <|>
           try forStatementParser <|>
           try assignParser <|>
           ExprStatement <$> expressionParser

    wsSkip

    -- Get semicolon at the end of the line.
    optional (char ';')

    wsSkip

    pure val

varDefParser :: CharParser st CStatement
varDefParser = do
    var <- varParser

    init <- optionMaybe initialization

    pure $ VarDef var init

    where initialization = do
            wsSkip
            char '='
            wsSkip
            expressionParser

varParser :: CharParser st Var
varParser = do
    varType <- typeParser
    varName <- cIdentifier

    return $ Var varType varName

returnParser :: CharParser st CStatement
returnParser = do
    wsSkip
    string "return"
    wsSkip
    Return <$> expressionParser

assignParser :: CharParser st CStatement
assignParser = do
    lhs <- try (Right <$> arrayAccessParser) <|>
           Left <$> cIdentifier
    wsSkip
    assignOp <- optionMaybe $ choice $ map (try . string . fst) cArithOps
    char '='
    wsSkip
    rhs <- expressionParser

    case assignOp of
        Nothing -> pure $ Assign Nothing lhs rhs
        Just opName -> pure $ Assign (lookup opName cArithOps) lhs rhs

whileStatementParser :: CharParser st CStatement
whileStatementParser = do
    wsSkip
    string "while"

    wsSkip
    condition <- between (char '(') (char ')') expressionParser
    wsSkip

    body <- block

    pure $ WhileStatement condition body

ifStatementParser :: CharParser st CStatement
ifStatementParser = do
    wsSkip
    string "if"

    wsSkip
    condition <- between (char '(') (char ')') expressionParser
    wsSkip

    body <- block

    -- Get other branches.
    branches <- optionMaybe $ (wsSkip >> string "else" >> wsSkip >> (try (ElseBlock <$> block) <|> try ifStatementParser))

    pure $ IfStatement condition branches body

forStatementParser :: CharParser st CStatement
forStatementParser = do
    wsSkip
    string "for"
    wsSkip
    (ini, cond, step) <- between (char '(') (char ')') forInformation

    body <- block

    pure $ ForStatement ini cond step body
    where
        forInformation = do
            ini <- statementParser
            wsSkip
            cond <- expressionParser
            wsSkip
            char ';'
            wsSkip
            step <- statementParser
            wsSkip

            pure (ini, cond, step)

wsSkip :: CharParser st ()
wsSkip = do
    _ <- many (oneOf " \t")

    pure ()

newlines :: CharParser st ()
newlines = many1 (wsSkip >> char '\n' >> wsSkip) >> pure ()

block :: CharParser st [CStatement]
block = do
    wsSkip
    res <- between (char '{') (char '}') innerBlock
    wsSkip
    pure res

    where innerBlock = do
            optional newlines
            statements <- sepEndBy statementParser newlines
            optional newlines
            pure statements

expressionParser :: CharParser st CExpression
expressionParser = try prefixParser <|>
                   try postfixParser <|>
                   try cArithParser <|>
                   try (VarRef <$> cIdentifier) <|>
                   try arrayAccessParser <|>
                   try funcCallParser <|>
                   LitInt . read <$> many1 digit

arrayAccessParser :: CharParser st CExpression
arrayAccessParser = do
    name <- cIdentifier
    expr <- between (char '[') (char ']') expressionParser

    pure $ CArrayAccess name expr

operandParser :: CharParser st CExpression
operandParser = try (between (char '(') (char ')') (expressionParser <|> prefixParser <|> postfixParser)) <|>
                try arrayAccessParser <|>
                try funcCallParser <|>
                try (VarRef <$> cIdentifier) <|>
                LitInt . read <$> many1 digit

funcCallParser :: CharParser st CExpression
funcCallParser = do
    funcName <- cIdentifier

    arguments <- between (char '(') (char ')') $ sepEndBy expressionParser (wsSkip >> char ',' >> wsSkip)

    pure $ FuncCall funcName arguments

cPrefixOps = [("++", PreIncrement), ("--", PreDecrement), ("!", PreNot), ("*", Dereference)]

prefixParser :: CharParser st CExpression
prefixParser = do
    op <- choice $ map (try . string . fst) cPrefixOps
    wsSkip
    var <- cIdentifier

    case lookup op cPrefixOps of
        Nothing -> fail $ "Unknown prefix operation: " ++ op
        Just prefixOp -> pure $ CPrefix prefixOp var

cPostfixOps = [("++", PostIncrement), ("--", PostDecrement)]

postfixParser :: CharParser st CExpression
postfixParser = do
    var <- cIdentifier
    wsSkip
    op <- choice $ map (try . string . fst) cPostfixOps

    case lookup op cPostfixOps of
        Nothing -> fail $ "Unknown postfix operation: " ++ op
        Just postfixOp -> pure $ CPostfix postfixOp var

cArithOps = [("*", Mult), ("/", Div), ("+", Add), ("-", Minus),
             (">=", CGTE), ("<=", CLTE), ("!=", CNE), ("==", CEQ),
             ("/", Div), ("%", Mod), ("||", Or), ("&&", And),
             ("|", OrBit), ("&", AndBit), ("<<", ShiftLeft), (">>", ShiftRight),
             ("^", Xor), (">", CGT), ("<", CLT)]

cArithBinary = map (second CBinaryOp) cArithOps

extractExprOp :: (CExpression, Maybe String) -> Maybe (CExpression, CExpression -> CExpression -> CExpression)
extractExprOp (expr, op) = (expr,) <$> (op >>= (`lookup` cArithBinary))

resolve :: [String] -> [(CExpression, Maybe String)] -> Maybe (CExpression, Maybe String)
resolve _ [] = Nothing
resolve _ [x] = Just x
resolve [] _ = Nothing
resolve (op:ops) arith =
    -- Split the arithmetic expression with the highest precedence operator.
    case findSplit (maybePred (== op) . snd) arith of
        Nothing -> resolve ops arith
        Just (left, (cur,_), right) ->
            let l = extractExprOp =<< resolve (op:ops) left
                r = resolve (op:ops) right in
                case lookup op cArithBinary of
                    Nothing -> Nothing
                    Just f -> let (new, lastOp) = maybe (cur, Nothing) (first (cur `f`)) r in
                                  maybe (Just (new, lastOp)) (\(expr, lf) -> Just (expr `lf` new, lastOp)) l

cArithParser :: CharParser st CExpression
cArithParser = do
    arith <- opParser

    case resolve (map fst cArithBinary) arith of
        Just (expr, Nothing) -> pure expr
        _ -> error $ "Could not resolve '" ++ show arith ++ "' into an expression."

opParser :: CharParser st [(CExpression, Maybe String)]
opParser = do
    a <- operandParser

    wsSkip
    nextOp <- optionMaybe $ choice $ map (try . string . fst) cArithBinary
    wsSkip

    case nextOp of
        Nothing -> pure [(a, Nothing)]
        Just op -> do
            rest <- optionMaybe opParser

            case rest of
                Nothing -> pure [(a,Just op)]
                Just vs -> pure $ (a,Just op):vs

