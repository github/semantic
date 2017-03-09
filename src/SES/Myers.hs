{-# LANGUAGE GADTs #-}
module SES.Myers where

import qualified Data.Vector as Vector
import Prologue

data MyersF a where
  MiddleSnake :: Vector.Vector a -> Vector.Vector a -> MyersF (Snake, EditDistance)

data Snake = Snake { x :: Int, y :: Int, u :: Int, v :: Int }

newtype EditDistance = EditDistance { unEditDistance :: Int }
