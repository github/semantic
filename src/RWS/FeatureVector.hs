{-# LANGUAGE BangPatterns, MagicHash #-}
module RWS.FeatureVector
( FeatureVector
, unFV
, unitVector
, addVectors
) where

import GHC.Prim
import GHC.Types
import System.Random.Mersenne.Pure64

data FeatureVector = FV !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double# !Double#

unFV :: FeatureVector -> [Double]
unFV !(FV d00 d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14)
  = [ D# d00, D# d01, D# d02, D# d03, D# d04, D# d05, D# d06, D# d07, D# d08, D# d09, D# d10, D# d11, D# d12, D# d13, D# d14 ]

-- | Computes a unit vector of the specified dimension from a hash.
unitVector :: Int -> FeatureVector
unitVector !hash =
  let !(D# d00, r00) = randomDouble (pureMT (fromIntegral hash))
      !(D# d01, r01) = randomDouble r00
      !(D# d02, r02) = randomDouble r01
      !(D# d03, r03) = randomDouble r02
      !(D# d04, r04) = randomDouble r03
      !(D# d05, r05) = randomDouble r04
      !(D# d06, r06) = randomDouble r05
      !(D# d07, r07) = randomDouble r06
      !(D# d08, r08) = randomDouble r07
      !(D# d09, r09) = randomDouble r08
      !(D# d10, r10) = randomDouble r09
      !(D# d11, r11) = randomDouble r10
      !(D# d12, r12) = randomDouble r11
      !(D# d13, r13) = randomDouble r12
      !(D# d14, _) = randomDouble r13
      !(D# one) = 1
      !invMagnitude = one /## sqrtDouble# (d00 *## d00 +## d01 *## d01 +## d02 *## d02 +## d03 *## d03 +## d04 *## d04 +## d05 *## d05 +## d06 *## d06 +## d07 *## d07 +## d08 *## d08 +## d09 *## d09 +## d10 *## d10 +## d11 *## d11 +## d12 *## d12 +## d13 *## d13 +## d14 *## d14)
  in FV (invMagnitude *## d00) (invMagnitude *## d01) (invMagnitude *## d02) (invMagnitude *## d03) (invMagnitude *## d04) (invMagnitude *## d05) (invMagnitude *## d06) (invMagnitude *## d07) (invMagnitude *## d08) (invMagnitude *## d09) (invMagnitude *## d10) (invMagnitude *## d11) (invMagnitude *## d12) (invMagnitude *## d13) (invMagnitude *## d14)

addVectors :: FeatureVector -> FeatureVector -> FeatureVector
addVectors !(FV a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) !(FV b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14) = FV (a0 +## b0) (a1 +## b1) (a2 +## b2) (a3 +## b3) (a4 +## b4) (a5 +## b5) (a6 +## b6) (a7 +## b7) (a8 +## b8) (a9 +## b9) (a10 +## b10) (a11 +## b11) (a12 +## b12) (a13 +## b13) (a14 +## b14)
