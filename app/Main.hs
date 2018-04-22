module Main where

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)

import System.Environment (getArgs)
import System.Console.GetOpt

import Text.ParserCombinators.Parsec

import Compiler.Compiler
import Compiler.CodeGenerator
import Parser
import LispParser
import LispCompiler

options :: [OptDescr String]
options = [Option ['o'] ["output"] (ReqArg id "FILE") "output file"]

main :: IO ()
main = do
    args <- getArgs

    case getOpt Permute options args of
        (o, [filename], []) -> do
            text <- if "lisp" `isSuffixOf` filename then
                        generateFile <$> (compile =<< (compileLisp <$> loadLispFile filename))
                    else
                        generateFile <$> (compile =<< loadFile filename)

            putStrLn text

            case o of
                [] -> writeFile "a.s" text
                [outfile] -> writeFile outfile text
        (_, [], []) -> putStrLn "No input files provided."
        (_, _, err) -> print err

