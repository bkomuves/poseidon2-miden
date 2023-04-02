
-- | The meta-language

{-# LANGUAGE BangPatterns, DeriveFunctor #-}
module Macro where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import Expr 
import Goldilocks
import Yield

--------------------------------------------------------------------------------

data Interpolated fill
  = String !String
  | Interp !String !fill !(Interpolated fill)
  deriving (Show, Functor)

type Block = [Statement]

data Statement
  = Miden (Interpolated Name)            -- ^ raw miden code
  | For   Name Expr Expr Block           -- ^ for loop [a..b-1]
  | IfS   Expr Block (Maybe Block)       -- ^ if-then-else
  | Set   Name Expr                      -- ^ assignment
  | NOP                                  -- ^ empty statement
  deriving Show

--------------------------------------------------------------------------------

interpolate :: Scope Val -> Interpolated Name -> String
interpolate scope = go where
  go (String str) = str
  go (Interp str name rest) = case Map.lookup name scope of
    Nothing  -> error $ "name `" ++ name ++ "` in interpolated string not in scope"
    Just val -> case val of
      VInt   i   -> str ++ show i                       ++ go rest
      VBool  b   -> str ++ if b then "1" else "0"       ++ go rest
      VField x   -> str ++ show (goldilocksAsInteger x) ++ go rest
      _          -> error "trying to string interpolate an invalid type"

--------------------------------------------------------------------------------

-- state = scope
-- yield = string
type Run a = Exec (Scope Val) String a

execStatements :: Block -> Run ()
execStatements = go where

  go :: Block -> Run ()
  go []          = return ()
  go (this:rest) = do
    goOne this
    go    rest

  goOne :: Statement -> Run ()
  goOne this = case this of
  
    NOP -> do
      return ()
      
    Miden interp -> do
      scope <- getState
      let str = interpolate scope interp
      yieldLine str

    Set name expr -> do
      scope <- getState
      let val = evaluate scope expr
      let scope' = Map.insert name val scope
      setState scope'

    IfS cond actThen mbActElse -> do
      scope <- getState
      let val = evaluate scope cond
      case val of
        VBool True  -> go actThen
        VBool False -> case mbActElse of
          Nothing      -> return ()
          Just actElse -> go actElse 
        _              -> error "fatal error: if statement with non-boolean condition"

    For var exprA exprB body -> do
      scope <- getState
      let valA = evaluate scope exprA
      let valB = evaluate scope exprB
      case (valA,valB) of
        (VInt a, VInt b) -> forM_ [a..b] $ \idx -> do
          setState $ Map.insert var (VInt idx) scope 
          go body       
        _ -> error "fatal error: for loop with non-integer bounds"
      setState scope
        
--------------------------------------------------------------------------------

