{-# LANGUAGE GADTs #-}
module SES.Myers where

import Data.These
import qualified Data.Vector as Vector
import Prologue

data MyersF a where
  SES :: [a] -> [a] -> MyersF [These a a]
  MiddleSnake :: Vector.Vector a -> Vector.Vector a -> MyersF (Snake, EditDistance)
  FindDPath :: EditDistance -> Diagonal -> MyersF Int

data Snake = Snake { x :: Int, y :: Int, u :: Int, v :: Int }

newtype EditDistance = EditDistance { unEditDistance :: Int }
newtype Diagonal = Diagonal { unDiagonal :: Int }
