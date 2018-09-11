{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (view, set, makeLenses)

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import Options.Applicative

import Compiler.Compiler
import Compiler.CodeGenerator
import Compiler.Types
import Parser
import LispParser
import LispCompiler
import Types

data Args = Args
    { _infile :: FilePath,
      _outfile :: Maybe FilePath,
      _optLevel :: Int }
    deriving Show
makeLenses ''Args

argParser :: Parser Args
argParser = Args
    <$> argument str (help "The file to compile.")
    <*> optional (strOption (long "output" <> short 'o' <> help "The file to write to."))
    <*> option auto (value 1 <> short 'O' <> help "The optimization level. Currently either 0 for no optimization or positive values for maximum optimization")

getArgs = execParser opts
    where
        opts = info (argParser <**> helper)
                (fullDesc <>
                 progDesc "CMips is a C and Lisp compile that outputs MIPS assembly." <>
                 header "CMips")

main :: IO ()
main = do
    args <- getArgs

    let filename = view infile args
    let optVal = view optLevel args

    compilationResult <-
            if "lisp" `isSuffixOf` filename then
                compileWith (set optimizeLevel optVal defaultCompileOptions) =<< (compileLisp <$> loadLispFile filename)
            else
                compileWith (set optimizeLevel optVal defaultCompileOptions) =<< loadFile filename

    case compilationResult of
        Left warnings -> do
            putStrLn "Warnings:"
            mapM_ print warnings
        Right compiledFile -> do
            let text = generateFile compiledFile

            putStrLn text

            case view outfile args of
                Nothing -> writeFile "a.s" text
                Just oname -> writeFile oname text

