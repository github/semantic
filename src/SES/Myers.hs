{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import Prologue hiding (for, State)

data MyersF element result where
  SES :: (a -> a -> Bool) -> EditGraph a -> MyersF a [These a a]
  LCS :: (a -> a -> Bool) -> EditGraph a -> MyersF a [a]
  MiddleSnake :: (a -> a -> Bool) -> EditGraph a -> MyersF a (Snake, EditDistance)
  FindDPath :: (a -> a -> Bool) -> EditGraph a -> Direction -> EditDistance -> Diagonal -> MyersF a Endpoint

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF element result where
  M :: MyersF a b -> StepF a b
  S :: State MyersState b -> StepF a b

type Myers a = Freer (StepF a)

data EditGraph a = EditGraph { as :: !(Vector.Vector a), bs :: !(Vector.Vector a) }
data Snake = Snake { xy :: Endpoint, uv :: Endpoint }

newtype EditDistance = EditDistance { unEditDistance :: Int }
newtype Diagonal = Diagonal { unDiagonal :: Int }
data Endpoint = Endpoint { x :: !Int, y :: !Int }
data Direction = Forward | Reverse


-- Evaluation

runMyers :: Myers a b -> b
runMyers = runAll $ MyersState (Vector.replicate 100 0) (Vector.replicate 100 0)
  where runAll state step = case runMyersStep state step of
          Left a -> a
          Right next -> uncurry runAll next

runMyersStep :: MyersState -> Myers a b -> Either b (MyersState, Myers a b)
runMyersStep state step = case step of
  Return a -> Left a
  Then step cont -> case step of
    M myers -> Right (state, decompose myers >>= cont)

    S Get -> Right (state, cont state)
    S (Put state') -> Right (state', cont ())


decompose :: MyersF a b -> Myers a b
decompose myers = case myers of
  LCS eq graph
    | null (as graph) || null (bs graph) -> return []
    | otherwise -> do
      (Snake xy uv, EditDistance d) <- middleSnake eq graph
      if d > 1 then do
        let (before, _) = divideGraph graph xy
        let (start, after) = divideGraph graph uv
        let (mid, _) = divideGraph start xy
        before' <- lcs eq before
        after' <- lcs eq after
        return $! before' <> toList (as mid) <> after'
      else if length (bs graph) > length (as graph) then
        return (toList (as graph))
      else
        return (toList (bs graph))

  SES eq graph
    | null (bs graph) -> return (This <$> toList (as graph))
    | null (as graph) -> return (That <$> toList (bs graph))
    | otherwise -> do
      return []

  MiddleSnake eq graph -> fmap (fromMaybe (error "bleah")) $
    for [0..maxD] $ \ d ->
      (<|>)
      <$> for [negate d, negate d + 2 .. d] (\ k -> do
        forwardEndpoint <- findDPath eq graph Forward (EditDistance d) (Diagonal k)
        backwardV <- gets backward
        let reverseEndpoint = backwardV `at` (maxD + k)
        if odd delta && k `inInterval` (delta - pred d, delta + pred d) && overlaps forwardEndpoint reverseEndpoint
          then return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d - 1))
          else continue)
      <*> for [negate d, negate d + 2 .. d] (\ k -> do
        reverseEndpoint <- findDPath eq graph Reverse (EditDistance d) (Diagonal (k + delta))
        forwardV <- gets forward
        let forwardEndpoint = forwardV `at` (maxD + k + delta)
        if even delta && k `inInterval` (negate d, d) && overlaps forwardEndpoint reverseEndpoint
          then return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d))
          else continue)
    where n = length (as graph)
          m = length (bs graph)
          delta = n - m
          maxD = (m + n) `ceilDiv` 2

  FindDPath eq (EditGraph as bs) Forward (EditDistance d) (Diagonal k) -> do
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

  FindDPath eq (EditGraph as bs) Reverse (EditDistance d) (Diagonal k) -> return (Endpoint 0 0)


-- Smart constructors

lcs :: (a -> a -> Bool) -> EditGraph a -> Myers a [a]
lcs eq graph = M (LCS eq graph) `Then` return

findDPath :: (a -> a -> Bool) -> EditGraph a -> Direction -> EditDistance -> Diagonal -> Myers a Endpoint
findDPath eq graph direction d k = M (FindDPath eq graph direction d k) `Then` return

middleSnake :: (a -> a -> Bool) -> EditGraph a -> Myers a (Snake, EditDistance)
middleSnake eq graph = M (MiddleSnake eq graph) `Then` return


-- Implementation details

data MyersState = MyersState { forward :: !(Vector.Vector Int), backward :: !(Vector.Vector Int) }

setForward :: Vector.Vector Int -> Myers a ()
setForward v = modify (\ s -> s { forward = v })

setBackward :: Vector.Vector Int -> Myers a ()
setBackward v = modify (\ s -> s { backward = v })

at :: Vector.Vector Int -> Int -> Endpoint
at v k = let x = v Vector.! k in Endpoint x (x - k)

overlaps :: Endpoint -> Endpoint -> Bool
overlaps (Endpoint x y) (Endpoint u v) = x - y == u - v && x <= u

inInterval :: Ord a => a -> (a, a) -> Bool
inInterval k (lower, upper) = k >= lower && k <= upper

for :: [a] -> (a -> Myers c (Maybe b)) -> Myers c (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

continue :: Myers b (Maybe a)
continue = return Nothing

ceilDiv :: Integral a => a -> a -> a
ceilDiv = (uncurry (+) .) . divMod

divideGraph :: EditGraph a -> Endpoint -> (EditGraph a, EditGraph a)
divideGraph (EditGraph as bs) (Endpoint x y) =
  ( EditGraph (Vector.slice 0  x              as) (Vector.slice 0  y              bs)
  , EditGraph (Vector.slice x (length as - x) as) (Vector.slice y (length bs - y) bs) )


-- Instances

instance MonadState MyersState (Myers a) where
  get = S Get `Then` return
  put a = S (Put a) `Then` return
