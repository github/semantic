{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import Prologue hiding (for, State)

data MyersF a where
  SES :: EditGraph a -> MyersF [These a a]
  LCS :: EditGraph a -> MyersF [a]
  MiddleSnake :: EditGraph a -> MyersF (Snake, EditDistance)
  FindDPath :: EditGraph a -> Direction -> EditDistance -> Diagonal -> MyersF Endpoint

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF a where
  M :: MyersF a -> StepF a
  S :: State MyersState a -> StepF a

type Myers = Freer StepF

data EditGraph a = EditGraph { as :: !(Vector.Vector a), bs :: !(Vector.Vector a), eq :: !(a -> a -> Bool) }
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
  LCS graph
    | null (as graph) || null (bs graph) -> return []
    | otherwise -> return []

  SES graph
    | null (bs graph) -> return (This <$> toList (as graph))
    | null (as graph) -> return (That <$> toList (bs graph))
    | otherwise -> do
      return []

  MiddleSnake graph -> fmap (fromMaybe (error "bleah")) $
    for [0..maxD] $ \ d ->
      (<|>)
      <$> for [negate d, negate d + 2 .. d] (\ k -> do
        forwardEndpoint <- findDPath graph Forward (EditDistance d) (Diagonal k)
        backwardV <- gets backward
        let reverseEndpoint = backwardV `at` (maxD + k)
        if odd delta && k `inInterval` (delta - pred d, delta + pred d) && overlaps forwardEndpoint reverseEndpoint
          then return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d - 1))
          else continue)
      <*> for [negate d, negate d + 2 .. d] (\ k -> do
        reverseEndpoint <- findDPath graph Reverse (EditDistance d) (Diagonal (k + delta))
        forwardV <- gets forward
        let forwardEndpoint = forwardV `at` (maxD + k + delta)
        if even delta && k `inInterval` (negate d, d) && overlaps forwardEndpoint reverseEndpoint
          then return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d))
          else continue)
    where n = length (as graph)
          m = length (bs graph)
          delta = n - m
          maxD = (m + n) `ceilDiv` 2

  FindDPath (EditGraph as bs eq) Forward (EditDistance d) (Diagonal k) -> do
    v <- gets forward
    let prev = v `at` (maxD + pred k)
    let next = v `at` (maxD + succ k)
    let xy = if k == negate d || k /= d && x prev < x next
          then next
          else let x' = succ (x prev) in Endpoint x' (x' - k)
    let Endpoint x' y' = slide xy
    setForward (v Vector.// [(maxD + k, x')])
    return (Endpoint x' y')
    where n = length as
          m = length bs
          maxD = (m + n) `ceilDiv` 2

          slide (Endpoint x y)
            | (as Vector.! x) `eq` (bs Vector.! y) = slide (Endpoint (succ x) (succ y))
            | otherwise = Endpoint x y

  FindDPath (EditGraph as bs eq) Reverse (EditDistance d) (Diagonal k) -> return (Endpoint 0 0)


-- Smart constructors

findDPath :: EditGraph a -> Direction -> EditDistance -> Diagonal -> Myers Endpoint
findDPath graph direction d k = M (FindDPath graph direction d k) `Then` return

middleSnake :: EditGraph a -> Myers (Snake, EditDistance)
middleSnake graph = M (MiddleSnake graph) `Then` return


-- Implementation details

data MyersState = MyersState { forward :: !(Vector.Vector Int), backward :: !(Vector.Vector Int) }

setForward :: Vector.Vector Int -> Myers ()
setForward v = modify (\ s -> s { forward = v })

setBackward :: Vector.Vector Int -> Myers ()
setBackward v = modify (\ s -> s { backward = v })

at :: Vector.Vector Int -> Int -> Endpoint
at v k = let x = v Vector.! k in Endpoint x (x - k)

overlaps :: Endpoint -> Endpoint -> Bool
overlaps (Endpoint x y) (Endpoint u v) = x - y == u - v && x <= u

inInterval :: Ord a => a -> (a, a) -> Bool
inInterval k (lower, upper) = k >= lower && k <= upper

for :: [a] -> (a -> Myers (Maybe b)) -> Myers (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

continue :: Myers (Maybe a)
continue = return Nothing

ceilDiv :: Integral a => a -> a -> a
ceilDiv = (uncurry (+) .) . divMod


-- Instances

instance MonadState MyersState Myers where
  get = S Get `Then` return
  put a = S (Put a) `Then` return
