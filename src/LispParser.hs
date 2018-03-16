module LispParser where

import Data.List (isInfixOf, intercalate, find, isPrefixOf)
import qualified Data.Map as Map
import Data.String.Utils (replace)

import System.Environment
import System.IO

import Text.ParserCombinators.Parsec

import Parser
import LispLanguage
import ParserUtil

loadLispFile :: String -> IO MinilispFile
loadLispFile filename = do
    stdLib <- loadFile "lib/std.c"

    contents <- filter (/= '\r') <$> readFile filename

    case parse (minilispFile stdLib) "Error: " contents of
      Left err -> error $ show err
      Right file -> pure file

minilispFile stdLib = MinilispFile stdLib <$> sepEndBy expr whitespace

expr :: CharParser st Expr
expr = comment <|> val <|> evalFunc <|> listParser

comment :: CharParser st Expr
comment = do
    char ';'
    many (noneOf "\r\n")
    return Comment

val :: CharParser st Expr
val = Quoted <$> quotedVal <|>
      try (IntVal <$> readNum) <|>
      try (FloatVal <$> float) <|>
      Identifier <$> identifier

int :: CharParser st Integer
int = do
    negative <- try (oneOf "+-") <|> return ' '
    let sign = if negative == '+' then ' ' else negative

    val <- many1 digit

    -- It has to end in a non-decimal point character, otherwise it's a float.
    notFollowedBy (char '.')

    return $ read $ sign : val

float :: CharParser st Float
float = do
    negative <- try (oneOf "+-") <|> return ' '
    let sign = if negative == '+' then ' ' else negative

    beforeVal <- many (oneOf "123456890")
    char '.'
    afterVal <- many (oneOf "1234567890")

    let before = if beforeVal == "" then "0" else beforeVal
    let after = if afterVal == "" then "0" else afterVal
    return $ read $ sign : (before ++ "." ++ after)

identifier :: CharParser st String
identifier = many1 (noneOf "()\r\n \t\'\"")

emptylist :: CharParser st Expr
emptylist = do
    char '('
    many (char ' ')
    char ')'
    return $ List []

quotedVal :: CharParser st String
quotedVal = do
    char '"'
    content <- many (try (string "\\\"" >> return '"') <|> noneOf "\"")
    char '"'
    return content

listParser :: CharParser st Expr
listParser = do
    char '\''
    List <$> between (char '(' >> wsSkip) (wsSkip >> char ')') (sepBy expr whitespace)

evalFunc :: CharParser st Expr
evalFunc = do
    char '('
    spaces
    val:vals <- sepBy1 expr whitespace
    spaces
    char ')'

    pure $ case val of
        Identifier "def" ->
            case vals of
                (Evaluate (Identifier fName) args:body) -> Define fName (getArgs args) body
                _ -> Evaluate val vals
        _ -> Evaluate val vals

    where
        getArgs = map (\(Identifier n) -> n)

