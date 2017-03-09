{-# LANGUAGE GADTs #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import Prologue

data MyersF a where
  SES :: [a] -> [a] -> MyersF [These a a]
  MiddleSnake :: Vector.Vector a -> Vector.Vector a -> MyersF (Snake, EditDistance)
  FindDPath :: Direction -> EditDistance -> Diagonal -> MyersF Endpoint

type Myers = Freer MyersF

data Snake = Snake { xy :: Endpoint, uv :: Endpoint }

newtype EditDistance = EditDistance { unEditDistance :: Int }
newtype Diagonal = Diagonal { unDiagonal :: Int }
newtype Endpoint = Endpoint { unEndpoint :: (Int, Int) }
data Direction = Forward | Reverse


decompose :: MyersF a -> Myers a
decompose myers = case myers of
  SES {} -> return []

  MiddleSnake {} -> return (Snake (Endpoint (0, 0)) (Endpoint (0, 0)), EditDistance 0)

  FindDPath {} -> return (Endpoint (0, 0))
