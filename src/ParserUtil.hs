module ParserUtil where

import Text.ParserCombinators.Parsec

readNum :: Num a => CharParser st a
readNum = do
    isNegative <- optionMaybe $ char '-'
    digits <- many1 digit

    let sign = maybe 1 (const (-1)) isNegative

    pure $ sign * fromInteger (read digits)

symbol :: CharParser st a -> CharParser st a
symbol parser = do
    wsSkip
    v <- parser
    wsSkip
    pure v

wsSkip :: CharParser st ()
wsSkip = do
    many (oneOf " \r\t")
    pure ()

whitespace :: CharParser st ()
whitespace = skipMany1 (oneOf " \t\n\r")

newlines :: CharParser st ()
newlines = many1 (wsSkip >> char '\n' >> wsSkip) >> pure ()

