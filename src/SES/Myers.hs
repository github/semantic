{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import Prologue

data MyersF a where
  SES :: [a] -> [a] -> MyersF [These a a]
  MiddleSnake :: Vector.Vector a -> Vector.Vector a -> MyersF (Snake, EditDistance)
  FindDPath :: Direction -> EditDistance -> Diagonal -> MyersF Endpoint

data StepF a where
  M :: MyersF a -> StepF a
  S :: State MyersState a -> StepF a

type Myers = Freer StepF

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


-- Implementation details

data MyersState = MyersState { forward :: !(Vector.Vector Int), backward :: !(Vector.Vector Int), offset :: Diagonal }

getK :: Direction -> Diagonal -> Myers Endpoint
getK direction diagonal = do
  state <- get
  let v = (case direction of { Forward -> forward ; Reverse -> backward }) state
  v `at` diagonal

at :: Vector.Vector Int -> Diagonal -> Myers Endpoint
at v (Diagonal k) = do
  Diagonal o <- gets offset
  return (Endpoint (v Vector.! o + k, 0))

overlaps :: Endpoint -> Endpoint -> Bool
overlaps (Endpoint (x, y)) (Endpoint (u, v)) = x - y == u - v && x <= u


-- Instances

instance MonadState MyersState Myers where
  get = S get `Then` return
  put a = S (put a) `Then` return
