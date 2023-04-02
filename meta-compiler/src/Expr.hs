
{-# LANGUAGE TypeApplications, DeriveFunctor, FlexibleInstances, StandaloneDeriving #-}
module Expr where

--------------------------------------------------------------------------------

import Goldilocks

--------------------------------------------------------------------------------

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type Name = String

data ExprF expr
  = Var Name
  | Kst Val
  | Arr [expr]
  -- arithmetic
  | Neg expr 
  | Add expr expr
  | Mul expr expr
  -- logical
  | Not expr
  | And expr expr
  | Or_ expr expr
  -- comparison
  | Lt_ expr expr
  | Le_ expr expr
  | Eq_ expr expr
  -- ifte
  | If_ expr expr expr
  -- indexing
  | Idx expr expr
  deriving (Eq,Show,Functor)

newtype Fix f 
  = MkFix (f (Fix f))

type Expr = Fix ExprF

deriving instance Eq   Expr
deriving instance Show Expr

--------------------------------------------------------------------------------

instance Num Expr where
  fromInteger = MkFix . Kst . VInt 
  negate x = MkFix (Neg x)
  (+) x y  = MkFix (Add x y)
  (-) x y  = MkFix (Add x (MkFix $ Neg y))
  (*) x y  = MkFix (Mul x y)
  abs    = error "Expr/Num: abs"
  signum = error "Expr/Num: signum"

--------------------------------------------------------------------------------

data Val
  = VField !Goldilocks
  | VInt   !Integer
  | VBool  !Bool
  | VArr   ![Val]
  deriving (Eq,Show)

type Scope val = Map Name val

type Scope_ = Scope Val

emptyScope :: Scope val
emptyScope = Map.empty

--------------------------------------------------------------------------------

evaluate :: Scope Val -> Expr -> Val
evaluate scope = go where

  go (MkFix expr) = case fmap go expr of

    Var name  -> case Map.lookup name scope of
                   Nothing -> error $ "name `" ++ name ++ "` not in scope"
                   Just y  -> y

    Kst val   -> val

    Arr vals  -> VArr vals

    Neg val1      -> case (val1) of 
      (VField x)           -> VField (negate x)
      (VInt   x)           -> VInt   (negate x)
      _                    -> error "negating a non-numeric value"

    Add val1 val2 -> case (val1,val2) of 
      (VField x, VField y) -> VField (x+y)
      (VInt   x, VInt   y) -> VInt   (x+y)
      _                    -> error "adding non-numeric values"

    Mul val1 val2 -> case (val1,val2) of 
      (VField x, VField y) -> VField (x*y)
      (VInt   x, VInt   y) -> VInt   (x*y)
      _                    -> error "multiplying non-numeric values"

    Not val1 -> case (val1) of 
      (VBool  x)           -> VBool (not x)
      _                    -> error "logical NOT on non-boolean"

    And val1 val2 -> case (val1,val2) of 
      (VBool  x, VBool  y) -> VBool (x && y)
      _                    -> error "logical AND on non-booleans"

    Or_ val1 val2 -> case (val1,val2) of 
      (VBool  x, VBool  y) -> VBool (x || y)
      _                    -> error "logical OR on non-booleans"

    Eq_ val1 val2 -> case (val1,val2) of 
      (VInt   x, VInt   y) -> VBool (x==y)
      (VField x, VField y) -> VBool (x==y)
      (VBool  x, VBool  y) -> VBool (x==y)
      _                    -> error "comparing non-compatible values (EQ)"

    Lt_ val1 val2 -> case (val1,val2) of 
      (VInt   x, VInt   y) -> VBool (x<y)
      _                    -> error "comparing non-integer values (LT)"

    Le_ val1 val2 -> case (val1,val2) of 
      (VInt   x, VInt   y) -> VBool (x<=y)
      _                    -> error "comparing non-integer values (LE)"

    If_ valcond val1 val2 -> case valcond of
      VBool b -> if b then val1 else val2
      _       -> error "non-boolean as first argument of if-then-else"

    Idx arr index -> case (arr,index) of
      (VArr xs, VInt idx) -> 
        let idx_ = fromInteger @Int idx in
        if idx_ < 0 || idx_ >= length xs
            then error $ "index out of bounds: " ++ show idx_
            else xs !! idx_
      _ -> error "invalid types when indexing an array"

--------------------------------------------------------------------------------
