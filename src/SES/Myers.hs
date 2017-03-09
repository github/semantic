{-# LANGUAGE GADTs #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import Prologue

data MyersF a where
  SES :: [a] -> [a] -> MyersF [These a a]
  MiddleSnake :: Vector.Vector a -> Vector.Vector a -> MyersF (Snake, EditDistance)
  FindDPath :: EditDistance -> Diagonal -> MyersF Int

type Myers = Freer MyersF

data Snake = Snake { x :: Int, y :: Int, u :: Int, v :: Int }

newtype EditDistance = EditDistance { unEditDistance :: Int }
newtype Diagonal = Diagonal { unDiagonal :: Int }


decompose :: MyersF a -> Myers a
decompose myers = case myers of
  SES _ _ -> return []

  MiddleSnake _ _ -> return (Snake 0 0 0 0, EditDistance 0)

  FindDPath _ _ -> return 0
