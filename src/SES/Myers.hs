{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import Prologue hiding (for, State)

data MyersF a where
  SES :: [a] -> [a] -> MyersF [These a a]
  MiddleSnake :: Vector.Vector a -> Vector.Vector a -> MyersF (Snake, EditDistance)
  FindDPath :: Direction -> EditDistance -> Diagonal -> MyersF Endpoint

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF a where
  M :: MyersF a -> StepF a
  S :: State MyersState a -> StepF a

type Myers = Freer StepF

data Snake = Snake { xy :: Endpoint, uv :: Endpoint }

newtype EditDistance = EditDistance { unEditDistance :: Int }
newtype Diagonal = Diagonal { unDiagonal :: Int }
data Endpoint = Endpoint { x :: !Int, y :: !Int }
data Direction = Forward | Reverse


-- Evaluation

runMyersStep :: MyersState -> Myers a -> Either a (MyersState, Myers a)
runMyersStep state step = case step of
  Return a -> Left a
  Then step cont -> case step of
    M myers -> Right (state, decompose myers >>= cont)

    S Get -> Right (state, cont state)
    S (Put state') -> Right (state', cont ())


decompose :: MyersF a -> Myers a
decompose myers = case myers of
  SES as [] -> return (This <$> as)
  SES [] bs -> return (That <$> bs)
  SES {} -> return []

  MiddleSnake as bs -> fmap (fromMaybe (error "bleah")) $
    for [0..maxD] $ \ d ->
      (<|>)
      <$> for [negate d, negate d + 2 .. d] (\ k -> do
        forwardEndpoint <- findDPath Forward (EditDistance d) (Diagonal k)
        backwardV <- gets backward
        let reverseEndpoint = backwardV `at` (maxD + k)
        if odd delta && k `inInterval` (delta - pred d, delta + pred d) && overlaps forwardEndpoint reverseEndpoint
          then return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d - 1))
          else continue)
      <*> for [negate d, negate d + 2 .. d] (\ k -> do
        reverseEndpoint <- findDPath Reverse (EditDistance d) (Diagonal (k + delta))
        forwardV <- gets forward
        let forwardEndpoint = forwardV `at` (maxD + k + delta)
        if even delta && k `inInterval` (negate d, d) && overlaps forwardEndpoint reverseEndpoint
          then return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d))
          else continue)

    where ceilDiv = (uncurry (+) .) . divMod
          n = length as
          m = length bs
          delta = n - m
          maxD = (m + n) `ceilDiv` 2

          for :: [a] -> (a -> Myers (Maybe b)) -> Myers (Maybe b)
          for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

          continue = return Nothing

  FindDPath {} -> return (Endpoint 0 0)


-- Smart constructors

findDPath :: Direction -> EditDistance -> Diagonal -> Myers Endpoint
findDPath direction d k = M (FindDPath direction d k) `Then` return

middleSnake :: Vector.Vector a -> Vector.Vector a -> Myers (Snake, EditDistance)
middleSnake as bs = M (MiddleSnake as bs) `Then` return


-- Implementation details

data MyersState = MyersState { forward :: !(Vector.Vector Int), backward :: !(Vector.Vector Int), offset :: Diagonal }

getK :: Direction -> Diagonal -> Myers Endpoint
getK direction (Diagonal diagonal) = do
  MyersState forward backward (Diagonal offset) <- get
  let v = case direction of { Forward -> forward ; Reverse -> backward }
  return $! v `at` (offset + diagonal)

at :: Vector.Vector Int -> Int -> Endpoint
at v k = Endpoint (v Vector.! k) 0

overlaps :: Endpoint -> Endpoint -> Bool
overlaps (Endpoint x y) (Endpoint u v) = x - y == u - v && x <= u

inInterval :: Ord a => a -> (a, a) -> Bool
inInterval k (lower, upper) = k >= lower && k <= upper


-- Instances

instance MonadState MyersState Myers where
  get = S Get `Then` return
  put a = S (Put a) `Then` return
