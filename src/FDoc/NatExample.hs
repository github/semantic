module FDoc.NatExample where

import Prologue hiding (Nat)
import Data.Functor.Foldable

-- Our base Functor. The recursive bit is parameterized by r.
data NatF r =
    ZeroF
  | SuccF r
  deriving (Show, Functor)

-- Fix represents the "fixed point" for the NatF Functor, and enables recursion.
-- Important to note this has kind * -> *.
type Nat = Fix NatF

-- This is a fully applied type (Has kind *).
zero' :: Nat
zero' = Fix ZeroF

-- This is a partially applied type (has kind * -> *). The recursive bit is used
-- by recursion schemes and is referred to as the "carrier" functor.
succ' :: Nat -> Nat
succ' = Fix . SuccF

-- Catamorphism: "tear down" a recursive structure in the shape of Nat.
natToIntCata :: Nat -> Int
natToIntCata nats = cata algebra nats
  where
    algebra term = case term of
      ZeroF -> 0
      SuccF value -> 1 + value

-- Anamorphism: "build up" a recursive structure in the shape of Nat.
intToNatAna :: Int -> Nat
intToNatAna num = ana coalgebra num
  where
    coalgebra num = case num of
      0 -> ZeroF
      _ -> SuccF (num - 1)

-- Hylomorphism: first apply an anamorphism and then a catamorphism in the shape
-- of Nat.
natHylo :: Int -> Int
natHylo num = hylo algebra coalgebra num
  where
    algebra term = case term of
      ZeroF -> 0
      SuccF value -> 1 + value
    coalgebra num = case num of
      0 -> ZeroF
      _ -> SuccF (num - 1)

-- Paramorphism: primitive recursion maintaining the original value along with
-- its computed value.
natPara :: Nat -> Int
natPara nats = para algebra nats
  where
    algebra value = case value of
      ZeroF -> 0
      (SuccF (_, value')) -> 1 + value'
