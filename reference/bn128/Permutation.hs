
-- | The Poseidon2 permutation

module Permutation where

--------------------------------------------------------------------------------

import RoundConsts
import State
import BN256

--------------------------------------------------------------------------------

sbox :: BN256 -> BN256
sbox x = x4*x where
  x2 = x *x
  x4 = x2*x2

internalRound :: BN256 -> Triple BN256 -> Triple BN256 
internalRound c (x,y,z) = 
  ( 2*x' +   y +   z 
  ,   x' + 2*y +   z 
  ,   x' +   y + 3*z 
  )
  where
    x' = sbox (x + c) 

externalRound :: Triple BN256 -> Triple BN256 -> Triple BN256
externalRound (cx,cy,cz) (x,y,z) = (x'+s , y'+s , z'+s) where
  x' = sbox (x + cx)
  y' = sbox (y + cy)
  z' = sbox (z + cz)
  s  = x' + y' + z'

linearLayer :: Triple BN256 -> Triple BN256
linearLayer (x,y,z) = (x+s, y+s, z+s) where s = x+y+z

--------------------------------------------------------------------------------

permutation :: Triple BN256 -> Triple BN256
permutation 
  = (\state -> foldl (flip externalRound) state finalRoundConsts   )
  . (\state -> foldl (flip internalRound) state internalRoundConsts)
  . (\state -> foldl (flip externalRound) state initialRoundConsts )
  . linearLayer

compression :: (BN256,BN256) -> BN256
compression (x,y) = case permutation (x,y,0) of (z,_,_) -> z

--------------------------------------------------------------------------------
