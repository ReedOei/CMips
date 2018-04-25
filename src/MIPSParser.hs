module MIPSParser where

import Data.List (find)
import Data.Maybe (fromMaybe, catMaybes)
import Data.String.Utils (replace)

import Text.ParserCombinators.Parsec

import MIPSLanguage
import ParserUtil

loadMIPSFile :: FilePath -> IO MIPSFile
loadMIPSFile path = do
    contents <- readFile path

    case parse (mipsFileParser path) "" contents of
        Left err -> error $ show err
        Right file -> pure file

mipsFileParser :: FilePath -> CharParser st MIPSFile
mipsFileParser fname = do
    dataSection <- optionMaybe $ try $ section "data"
    textSection <- optionMaybe $ try $ section "text"

    instrs <- sepEndBy mipsLine newlines

    pure $ MIPSFile fname (catMaybes [dataSection, textSection]) [instrs]

mipsLine = try mipsComment <|> try mipsLabel <|> try mipsEmpty <|> mipsInstr

mipsComment :: CharParser st MIPSInstruction
mipsComment = do
    wsSkip
    char '#'
    Comment <$> many (noneOf "\n")

mipsEmpty :: CharParser st MIPSInstruction
mipsEmpty = do
    wsSkip
    lookAhead $ char '\n'
    pure Empty

section :: String -> CharParser st MIPSSection
section name = do
    symbol $ char '.'
    string name
    newlines
    MIPSSection name <$> sepEndBy (try sectionElement) newlines

sectionElement :: CharParser st (String, String, String)
sectionElement = do
    wsSkip
    name <- many $ noneOf ":\n"
    symbol $ char ':'
    symbol $ char '.'
    dataType <- many $ noneOf " \n"
    wsSkip

    dataVal <- case dataType of
                "asciiz" -> do
                    str <- between (wsSkip >> char '\"') (char '\"' >> wsSkip) $ many $ noneOf "\n\""
                    pure $ replace "\\\\" "\\" $ replace "\\n" "\n" str
                _ -> many $ noneOf "\n"

    pure (name, dataType, dataVal)

opParser :: CharParser st MIPSOp
opParser = do
    opName <- many1 (noneOf " \n")
    case find ((== opName) . mnemonic) opList of
        Nothing -> error $ "Unknown mips mnemonic: '" ++ opName ++ "'"
        Just op -> pure op

operandParser :: CharParser st String
operandParser = do
    wsSkip
    optional (char '$')
    many (noneOf " (),\n")

mipsLabel :: CharParser st MIPSInstruction
mipsLabel = do
    wsSkip
    name <- many1 (noneOf ":\n")
    char ':'
    pure $ Label name

mipsInstr :: CharParser st MIPSInstruction
mipsInstr = do
    wsSkip
    op <- opParser

    case op of
        _ | op `elem` [OP_LW, OP_SW, OP_LB, OP_SB] -> do
            a <- operandParser
            symbol $ char ','

            b <- operandParser

            p <- optionMaybe $ lookAhead $ symbol $ char '('
            case p of
                -- No (, that means it's just a single val that goes in c, and a 0 offset
                Nothing -> pure $ Inst op a "0" b
                Just _ -> do
                    c <- between (symbol (char '(')) (symbol (char ')')) operandParser

                    pure $ Inst op a b c

        _ -> do
            a <- operandParser
            b <- optionMaybe $ do
                    symbol $ char ','
                    operandParser
            c <- optionMaybe $ do
                    symbol $ char ','
                    operandParser

            pure $ Inst op a (fromMaybe "" b) (fromMaybe "" c)

