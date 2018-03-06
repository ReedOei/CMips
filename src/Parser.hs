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
             try structParser <|>
             pure MiscElement

cIdentifier :: CharParser st String
cIdentifier = do
    first <- letter
    rest <- many (alphaNum <|> oneOf "_") -- Allow : for scope resolution and ~ for destructors

    pure $ first:rest

commentParser :: CharParser st CStatement
commentParser = try blockCommentParser <|> try (do
    wsSkip
    string "//"
    text <- many (noneOf "\n")
    pure $ CComment text)

blockCommentParser :: CharParser st CStatement
blockCommentParser = do
    wsSkip
    string "/*"
    text <- manyTill anyChar (string "*/")

    pure $ CComment text

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

structParser :: CharParser st CElement
structParser = do
    wsSkip
    optional $ string "typename"
    wsSkip
    string "struct"

    wsSkip
    name <- cIdentifier
    wsSkip

    varDefs <- block

    wsSkip
    optional cIdentifier
    wsSkip

    char ';' -- block ends with }, but structs end with };

    pure $ StructDef name $ map (\(VarDef var _) -> var) varDefs

functionParser :: CharParser st CElement
functionParser = do
    returnType <- typeParser
    wsSkip

    funcName <- cIdentifier

    arguments <- between (char '(') (char ')') $ sepBy (try varParser) (char ',')

    wsSkip
    next <- lookAhead (choice [char ';', char '{'])

    if next == ';' then do
        char ';'

        pure $ FuncDef returnType funcName arguments []
    else do
        body <- block

        pure $ FuncDef returnType funcName arguments body

typeParser :: CharParser st Type
typeParser = do
    wsSkip
    typeName <- do
        mod <- many (choice [string "struct", string "long", string "unsigned"] >>= (\str -> char ' ' >> pure str))
        t <- cIdentifier
        notFollowedBy (char '(')
        return $ unwords $ mod ++ [t]

    wsSkip
    varKindStr <- optionMaybe $ many $ char '*'
    wsSkip

    let varKind = case varKindStr of
                      Just ('*':_) -> Pointer
                      _ -> Value

    return $ Type varKind $ NamedType typeName

statementParser :: CharParser st CStatement
statementParser = do
    wsSkip
    val <- try returnParser <|>
           try varDefParser <|>
           try whileStatementParser <|>
           try ifStatementParser <|>
           try forStatementParser <|>
           try assignParser <|>
           try commentParser <|>
           ExprStatement <$> expressionParser

    wsSkip
    -- Get semicolon at the end of the line.
    optional (char ';')
    optional commentParser

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
    lhs <- try expressionParser
    wsSkip
    assignOp <- optionMaybe $ choice $ map (try . string . fst) cArithOps
    char '='
    wsSkip
    rhs <- expressionParser

    case assignOp of
        Nothing -> pure $ Assign Nothing lhs rhs
        Just opName -> pure $ Assign (lookup opName cArithOps) lhs rhs

conditionParser :: CharParser st CExpression
conditionParser = do
    wsSkip
    condition <- between (char '(') (char ')') expressionParser
    wsSkip

    pure condition

whileStatementParser :: CharParser st CStatement
whileStatementParser = do
    wsSkip
    string "while"

    condition <- conditionParser

    body <- block

    pure $ WhileStatement condition body

ifStatementParser :: CharParser st CStatement
ifStatementParser = do
    wsSkip
    string "if"

    condition <- conditionParser

    body <- block

    -- Get other branches.
    branches <- optionMaybe (wsSkip >> string "else" >> wsSkip >> (try (ElseBlock <$> block) <|> try ifStatementParser))

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

readNum :: Num a => CharParser st a
readNum = do
    isNegative <- optionMaybe $ char '-'
    digits <- many1 digit

    let sign = maybe 1 (const (-1)) isNegative

    pure $ sign * fromInteger (read digits)

expressionParser :: CharParser st CExpression
expressionParser =
                   try postfixParser <|>
                   try cArithParser <|>
                   try prefixParser <|>
                   try memberAccessParser <|>
                   try nullParser <|>
                   try (VarRef <$> cIdentifier) <|>
                   try arrayAccessParser <|>
                   try funcCallParser <|>
                   try charParser <|>
                   LitInt <$> readNum

accessOperandParser :: CharParser st CExpression
accessOperandParser = try (between (char '(') (char ')') (try expressionParser <|> try prefixParser <|> try postfixParser)) <|>
                      try arrayAccessParser <|>
                      try funcCallParser <|>
                      try nullParser <|>
                      try (VarRef <$> cIdentifier) <|>
                      try charParser <|>
                      LitInt <$> readNum

memberAccessParser :: CharParser st CExpression
memberAccessParser = do
    wsSkip
    a <- accessOperandParser
    accessOpStr <- string "." <|> string "->"
    b <- try memberAccessParser <|>
         try (VarRef <$> cIdentifier)

    case accessOpStr of
        "." -> pure $ MemberAccess a b
        "->" -> pure $ MemberAccess (CPrefix Dereference a) b

nullParser :: CharParser st CExpression
nullParser = string "NULL" >> pure NULL

charParser :: CharParser st CExpression
charParser = do
    wsSkip
    char '\''
    c <- anyChar
    char '\''
    wsSkip

    pure $ LitChar c

arrayAccessParser :: CharParser st CExpression
arrayAccessParser = do
    name <- cIdentifier
    expr <- between (char '[') (char ']') expressionParser

    pure $ CArrayAccess name expr

operandParser :: CharParser st CExpression
operandParser = try memberAccessParser <|>
                try prefixParser <|>
                try postfixParser <|>
                try (between (char '(') (char ')') (try expressionParser)) <|>
                try arrayAccessParser <|>
                try funcCallParser <|>
                try nullParser <|>
                try (VarRef <$> cIdentifier) <|>
                try charParser <|>
                LitInt <$> readNum

funcCallParser :: CharParser st CExpression
funcCallParser = do
    funcName <- cIdentifier

    arguments <- between (char '(') (char ')') $ sepEndBy expressionParser (wsSkip >> char ',' >> wsSkip)

    pure $ FuncCall funcName arguments

prefixOperandParser :: CharParser st CExpression
prefixOperandParser = try memberAccessParser <|>
                      try postfixParser <|>
                      try (between (char '(') (char ')') (try expressionParser)) <|>
                      try arrayAccessParser <|>
                      try funcCallParser <|>
                      try nullParser <|>
                      try (VarRef <$> cIdentifier) <|>
                      try charParser <|>
                      LitInt <$> readNum

prefixParser :: CharParser st CExpression
prefixParser = do
    op <- choice $ map (try . string . fst) cPrefixOps
    wsSkip
    var <- prefixOperandParser

    case lookup op cPrefixOps of
        Nothing -> fail $ "Unknown prefix operation: " ++ op
        Just prefixOp -> pure $ CPrefix prefixOp var

postfixOperandParser :: CharParser st CExpression
postfixOperandParser = try memberAccessParser <|>
                       try prefixParser <|>
                       try (between (char '(') (char ')') (try expressionParser)) <|>
                       try arrayAccessParser <|>
                       try funcCallParser <|>
                       try nullParser <|>
                       try (VarRef <$> cIdentifier) <|>
                       try charParser <|>
                       LitInt <$> readNum


postfixParser :: CharParser st CExpression
postfixParser = do
    var <- postfixOperandParser
    wsSkip
    op <- choice $ map (try . string . fst) cPostfixOps

    case lookup op cPostfixOps of
        Nothing -> fail $ "Unknown postfix operation: " ++ op
        Just postfixOp -> pure $ CPostfix postfixOp var

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

