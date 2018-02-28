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

import Util (findSplit, maybePred)

data VarKind = Pointer | Value
    deriving (Show, Eq)

data PreKind = Include
             | IfNDef
             | EndIf
             | Define
             | MiscPreKind
    deriving (Show, Eq)

data Type = Type Bool VarKind String
    deriving (Show, Eq)

data Var = Var Type String
    deriving (Show, Eq)

data CElement = Preprocessor PreKind String
                | FuncDef Type String [Var] Bool [CStatement]
                | MiscElement
    deriving (Show, Eq)

data CStatement = Return CExpression
                | VarDef Var
    deriving (Show, Eq)

data CExpression = VarRef String
                 | LitInt Int
                 | CAdd CExpression CExpression
                 | CMinus CExpression CExpression
                 | CGT CExpression CExpression
                 | CLT CExpression CExpression
                 | CTestEq CExpression CExpression
                 | CLTE CExpression CExpression
                 | CGTE CExpression CExpression
                 | CNE CExpression CExpression
                 | CMult CExpression CExpression
                 | CDiv CExpression CExpression
                 | CMod CExpression CExpression
    deriving (Show, Eq)

data CFile = CFile String [CElement]
    deriving Show

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
    spaces
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

    spaces

    val <- many (noneOf "\n")

    return $ Preprocessor preKind val

isConst :: CharParser st Bool
isConst = do
    spaces
    constM <- optionMaybe $ string "const"
    spaces

    return $ isJust constM

functionParser :: CharParser st CElement
functionParser = do
    returnType <- typeParser
    spaces

    funcName <- cIdentifier

    arguments <- between (char '(') (char ')') $ sepBy (try varParser) (char ',')

    spaces

    -- const <- isConst
    spaces
    next <- lookAhead (choice [char ';', char '{'])

    if next == ';' then do
        char ';'

        pure $ FuncDef returnType funcName arguments False []
    else do
        body <- between (char '{') (char '}') $ sepEndBy statementParser (many1 (char '\n'))

        pure $ FuncDef returnType funcName arguments False body

typeParser :: CharParser st Type
typeParser = do
    -- const <- isConst

    spaces
    typeName <- do
        t <- cIdentifier
        notFollowedBy (char '(')
        return t
    spaces

    spaces
    varKindStr <- optionMaybe $ char '*'
    spaces

    let varKind = case varKindStr of
                      Just '*' -> Pointer
                      Nothing -> Value

    return $ Type False varKind typeName

statementParser :: CharParser st CStatement
statementParser = do
    val <- try (Return <$> returnParser) <|>
                  VarDef <$> varParser

    -- Get semicolon at the end of the line.
    char ';'

    pure val

varParser :: CharParser st Var
varParser = do
    varType <- typeParser
    varName <- cIdentifier

    return $ Var varType varName

returnParser :: CharParser st CExpression
returnParser = do
    spaces
    string "return"
    spaces
    expressionParser

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

    spaces
    nextOp <- optionMaybe $ choice $ map (try . string . fst) cArithOps
    spaces

    case nextOp of
        Nothing -> pure [(a, Nothing)]
        Just op -> do
            rest <- optionMaybe opParser

            case rest of
                Nothing -> pure [(a,Just op)]
                Just vs -> pure $ (a,Just op):vs

