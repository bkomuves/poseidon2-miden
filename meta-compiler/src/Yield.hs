
-- | Yield monad

{-# LANGUAGE BangPatterns, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Yield where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.State.Strict

--------------------------------------------------------------------------------
-- * Yield monad to collect solutions

newtype Yield y a 
  = Yield (State [y] a)
  deriving (Functor,Applicative,Monad)

yield :: y -> Yield y ()
yield !what = Yield $ do
  old <- get
  put (what:old)   

runYield :: Yield y () -> [y]
runYield (Yield action) = execState action []

--------------------------------------------------------------------------------
-- * Exec monad with a state and collection

newtype Exec s y a 
  = Exec (StateT s (Yield y) a)
  deriving (Functor,Applicative,Monad)

yieldLine :: y -> Exec s y ()
yieldLine line = Exec (lift $ yield line)

getState :: Exec s y s
getState = Exec get

setState :: s -> Exec s y ()
setState s = Exec (put s)

modifyState :: (s -> s) -> Exec s y ()
modifyState f = Exec (modify f)

runExecMonad :: Exec s y () -> s -> [y]
runExecMonad (Exec action) iniState = reverse $ runYield (evalStateT action iniState) 

--------------------------------------------------------------------------------
