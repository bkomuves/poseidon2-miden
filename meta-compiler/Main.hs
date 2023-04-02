
-- { "operand_stack": ["10","11","12","13","14","15","16","17"] }
-- { "operand_stack": ["0","1","2","3","4","5","6","7"] }




module Main where

--------------------------------------------------------------------------------

import System.FilePath
import System.Environment

import Expr
import Goldilocks
import Yield
import Macro
import Parser

--------------------------------------------------------------------------------

help = do
  putStrLn "usage:"
  putStrLn "$ meta-compiler <source.meta>"

compile fname = do
  ei <- parseFile "poseidon2.meta"

  let prg = case ei of
        Left  err -> error err
        Right prg -> prg

  let out = runExecMonad (execStatements prg) emptyScope

  let asm = unlines out

  -- putStrLn "======================"
  -- putStrLn $ asm

  let outFile = takeBaseName fname <.> "masm"
  
  writeFile outFile asm

--------------------------------------------------------------------------------

main = do

  args <- getArgs
  case args of
    [fn] -> compile fn
    _    -> help

