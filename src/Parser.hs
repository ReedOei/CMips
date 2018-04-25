{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Arrow
import Control.Monad

import Data.List (tails)
import Data.Maybe (isJust, maybe)
import Data.String.Utils (replace)

import Text.ParserCombinators.Parsec
import Text.Parsec.Expr

import System.IO.Unsafe

import CLanguage
import ParserUtil

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
           try inlineParser <|>
           try (CommentElement <$> commentParser) <|>
           pure MiscElement

float :: CharParser st Float
float = do
    negative <- optionMaybe $ oneOf "+-"

    beforeVal <- many (oneOf "123456890")
    char '.'
    afterVal <- many (oneOf "1234567890")

    let before = if beforeVal == "" then "0" else beforeVal
    let after = if afterVal == "" then "0" else afterVal

    case negative of
        Just sign -> pure $ read $ sign : (before ++ "." ++ after)
        Nothing -> pure $ read $ before ++ "." ++ after

inlineParser :: CharParser st CElement
inlineParser = do
    string "mips"
    wsSkip

    returnType <- typeParser
    wsSkip

    funcName <- cIdentifier

    arguments <- between (char '(') (char ')') $ sepBy (try varParser) (char ',')

    wsSkip
    next <- lookAhead (choice [char ';', char '{'])

    if next == ';' then do
        char ';'

        pure $ Inline returnType funcName arguments []
    else do
        body <- between (char '{') (char '}') $ sepEndBy (many (noneOf "}\n\r")) newlines

        pure $ Inline returnType funcName arguments $ filter (not . null) body

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
                    "define" -> CDefine
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
        mod <- many (choice (map try [string "struct", string "long", string "unsigned"]) >>= (\str -> char ' ' >> pure str))
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

-- typeParser :: CharParser st Type
-- typeParser = do
--     wsSkip
--     typeName <- do
--         isStruct <- optionMaybe $ try $ string "struct"
--         wsSkip
--         isUnsigned <- optionMaybe $ try $ string "unsigned"
--         wsSkip
--         isLong <- optionMaybe $ try $ string "long"
--         wsSkip

--         t <- optionMaybe $ try $ do
--             baseName <- cIdentifier
--             lookAhead (wsSkip >> optionMaybe (many (char '*')) >> wsSkip >> cIdentifier)

--             pure baseName

--         notFollowedBy (char '(')

--         return $ unwords $ catMaybes [isStruct, isUnsigned, isLong, t]

--     wsSkip
--     varKindStr <- optionMaybe $ many $ char '*'
--     wsSkip

--     pure $ case varKindStr of
--               Just pointers@('*':_) -> foldl go (NamedType typeName) pointers
--               _ -> Type Value $ NamedType typeName

--     where
--         go t '*' = Type Pointer t

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
    (varKind, varName) <- varNameParser

    arguments <- optionMaybe $ between (char '(') (char ')') $ sepBy (try varParser) (char ',')
    arrSize <- optionMaybe (read <$> between (char '[') (char ']') (many digit))

    let tempType =
            case arguments of
                Nothing -> varType
                Just args ->
                    if varKind == Pointer then
                        let argTypes = map (\(Var varType _) -> varType) args in
                            FunctionPointer varType argTypes
                    else
                        error "Cannot have arguments to a variable that is not a function pointer!"

    case arrSize of
        Nothing -> pure $ Var tempType varName
        Just arrSize -> pure $ Var (Array arrSize tempType) varName

    where
        innerParser = do
            isPointer <- char '*'
            name <- cIdentifier
            pure (Just isPointer, name)

        varEndParser = do
            r <- cIdentifier <|> (:[]) <$> lookAhead (oneOf ";,)")

            pure $ case r of
                ";" -> ""
                "," -> ""
                ")" -> ""
                _ -> r

        varNameParser = do
            (isPointer, name) <- try (between (char '(') (char ')') innerParser) <|>
                                 (Nothing,) <$> varEndParser

            pure $ case isPointer of
                Nothing -> (Value, name)
                Just '*' -> (Pointer, name)


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

block :: CharParser st [CStatement]
block = do
    wsSkip
    optional newlines
    res <- between (char '{') (char '}') innerBlock
    wsSkip
    pure res

    where innerBlock = do
            optional newlines
            statements <- sepEndBy statementParser newlines
            optional newlines
            pure statements

expressionParser :: CharParser st CExpression
expressionParser =
                   try cArithParser <|>
                   try nullParser <|>
                   try (VarRef <$> cIdentifier) <|>
                   try funcCallParser <|>
                   try charParser <|>
                   try stringParser <|>
                   try (LitFloat <$> float) <|>
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

stringParser :: CharParser st CExpression
stringParser = do
    str <- between (char '"') (char '"') (many (noneOf "\""))

    -- Make sure escaped sequences aren't TOO escaped.
    pure $ LitString $ replace "\\\\" "\\" $ replace "\\n" "\n" str

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
                try stringParser <|>
                try (LitFloat <$> float) <|>
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
        [prefix "++" PreIncrement, prefix "--" PreDecrement, prefix "*" Dereference, prefix "!" PreNot, prefix "&" AddressOf],
        [postfix "++" PostIncrement, postfix "--" PostDecrement],
        [binary "<<" ShiftLeft, binary ">>" ShiftRight],
        [binary "*" Mult, binary "/" Div, binary "%" Mod],
        [binary "+" Add, binary "-" Minus],
        [binary ">=" CGTE, binary "<=" CLTE, binary ">" CGT, binary "<" CLT, binary "==" CEQ, binary "!=" CNE],
        [binary "&&" And, binary "||" Or],
        [binary "^" Xor, binary "&" AndBit, binary "|" OrBit]
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

