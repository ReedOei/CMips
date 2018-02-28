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

loadFile :: String -> IO (Maybe CFile)
loadFile filename = do
    contents <- filter (/= '\r') <$> readFile filename

    case parse (fileParser filename) "Error: " contents of
      Left err -> do
          print err
          return Nothing
      Right file -> return $ Just file

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
           try assignParser <|>
           ifStatementParser

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
    lhs <- cIdentifier
    wsSkip
    char '='
    wsSkip
    rhs <- expressionParser

    pure $ Assign Normal lhs rhs

ifStatementParser = conditionStatementParser "if" IfStatement
whileStatementParser = conditionStatementParser "while" WhileStatement

conditionStatementParser :: String -> (CExpression -> [CStatement] -> CStatement) -> CharParser st CStatement
conditionStatementParser cond constructor = do
    wsSkip
    string cond

    wsSkip
    condition <- between (char '(') (char ')') expressionParser
    wsSkip

    body <- block

    pure $ constructor condition body

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
expressionParser = try cArithParser <|>
                   try (VarRef <$> cIdentifier) <|>
                   LitInt . read <$> many1 digit

operandParser :: CharParser st CExpression
operandParser = try (between (char '(') (char ')') expressionParser) <|>
                try (VarRef <$> cIdentifier) <|>
                LitInt . read <$> many1 digit

cArithOps = [("%", CMod), ("*", CMult), ("/", CDiv), ("+", CAdd), ("-", CMinus),
             (">", CGT), ("<", CLT), ("==", CTestEq), ("<=", CLTE), (">=", CGTE), ("!=", CNE)]

extractExprOp :: (CExpression, Maybe String) -> Maybe (CExpression, CExpression -> CExpression -> CExpression)
extractExprOp (expr, op) = (expr,) <$> (op >>= (`lookup` cArithOps))

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
                case lookup op cArithOps of
                    Nothing -> Nothing
                    Just f -> let (new, lastOp) = maybe (cur, Nothing) (first (cur `f`)) r in
                                  maybe (Just (new, lastOp)) (\(expr, lf) -> Just (expr `lf` new, lastOp)) l

cArithParser :: CharParser st CExpression
cArithParser = do
    arith <- opParser

    case resolve (map fst cArithOps) arith of
        Just (expr, Nothing) -> pure expr
        _ -> error $ "Could not resolve '" ++ show arith ++ "' into an expression."

opParser :: CharParser st [(CExpression, Maybe String)]
opParser = do
    a <- operandParser

    wsSkip
    nextOp <- optionMaybe $ choice $ map (try . string . fst) cArithOps
    wsSkip

    case nextOp of
        Nothing -> pure [(a, Nothing)]
        Just op -> do
            rest <- optionMaybe opParser

            case rest of
                Nothing -> pure [(a,Just op)]
                Just vs -> pure $ (a,Just op):vs

