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

import Data.List (tails)
import Data.Maybe (isJust, maybe)

import Text.ParserCombinators.Parsec
import Text.Parsec.Expr

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

    pure $ StructDef name $ concatMap go varDefs
    where
        go (VarDef var _) = [var]
        go _ = []

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

    pure $ case varKindStr of
              Just pointers@('*':_) -> foldl go (NamedType typeName) pointers
              _ -> Type Value $ NamedType typeName

    where
        go t '*' = Type Pointer t


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
    optional $ char ';'
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
    lhs <- expressionParser
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
                   try cArithParser <|>
                   try nullParser <|>
                   try (VarRef <$> cIdentifier) <|>
                   try funcCallParser <|>
                   try charParser <|>
                   LitInt <$> readNum

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

arrayAccessOperator = Postfix parser
    where parser = try $ do
                exprs <- many1 (between (char '[') (char ']') expressionParser)

                pure $ \accessExpr -> foldl CArrayAccess accessExpr exprs

operandParser :: CharParser st CExpression
operandParser =
                try (between (char '(') (char ')') (try expressionParser)) <|>
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

operatorTable =
    [
        [cBinary "->" (\a b -> MemberAccess (CPrefix Dereference a) b), cBinary "." MemberAccess],
        [arrayAccessOperator],
        [prefix "++" PreIncrement, prefix "--" PreDecrement, prefix "*" Dereference, prefix "!" PreNot],
        [postfix "++" PostIncrement, postfix "--" PostDecrement],
        [binary "*" Mult, binary "/" Div, binary "%" Mod],
        [binary "+" Add, binary "-" Minus],
        [binary ">=" CGTE, binary "<=" CLTE, binary ">" CGT, binary "<" CLT, binary "==" CEQ, binary "!=" CNE],
        [binary "^" Xor, binary "<<" ShiftLeft, binary ">>" ShiftRight, binary "&" AndBit, binary "|" OrBit],
        [binary "&&" And, binary "||" Or]
    ]

opParser :: String -> (a -> b) -> CharParser st (a -> b)
opParser name fun = try $ do
    wsSkip
    string name
    wsSkip
    pure fun

cBinary name fun = Infix (opParser name fun) AssocLeft
binary name fun = Infix (opParser name (CBinaryOp fun)) AssocLeft
prefix name fun = Prefix (opParser name (CPrefix fun))
postfix name fun = Postfix (opParser name (CPostfix fun))

cArithParser :: CharParser st CExpression
cArithParser = buildExpressionParser operatorTable operandParser

