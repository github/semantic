{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.These
import qualified Data.Vector as Vector
import GHC.Stack
import Prologue hiding (for, State)

data MyersF element result where
  SES :: EditGraph a -> MyersF a [These a a]
  LCS :: EditGraph a -> MyersF a [a]
  MiddleSnake :: EditGraph a -> MyersF a (Snake, EditDistance)
  FindDPath :: EditGraph a -> Direction -> EditDistance -> Diagonal -> MyersF a Endpoint

editGraph :: MyersF a b -> EditGraph a
editGraph myers = case myers of
  SES g -> g
  LCS g -> g
  MiddleSnake g -> g
  FindDPath g _ _ _ -> g

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF element result where
  M :: MyersF a b -> StepF a b
  S :: State MyersState b -> StepF a b
  GetEq :: StepF a (a -> a -> Bool)

type Myers a = Freer (StepF a)

data EditGraph a = EditGraph { as :: !(Vector.Vector a), bs :: !(Vector.Vector a) }
data Snake = Snake { xy :: Endpoint, uv :: Endpoint }

newtype EditDistance = EditDistance { unEditDistance :: Int }
newtype Diagonal = Diagonal { unDiagonal :: Int }
data Endpoint = Endpoint { x :: !Int, y :: !Int }
data Direction = Forward | Reverse


-- Evaluation

runMyers :: HasCallStack => (a -> a -> Bool) -> Myers a b -> b
runMyers eq = runAll $ MyersState (Vector.replicate 100 0) (Vector.replicate 100 0)
  where runAll state step = case runMyersStep eq state step of
          Left a -> a
          Right next -> uncurry runAll next

runMyersStep :: HasCallStack => (a -> a -> Bool) -> MyersState -> Myers a b -> Either b (MyersState, Myers a b)
runMyersStep eq state step = let ?callStack = popCallStack callStack in case step of
  Return a -> Left a
  Then step cont -> case step of
    M myers -> Right (state, decompose myers >>= cont)

    S Get -> Right (state, cont state)
    S (Put state') -> Right (state', cont ())

    GetEq -> Right (state, cont eq)


decompose :: HasCallStack => MyersF a b -> Myers a b
decompose myers = let ?callStack = popCallStack callStack in case myers of
  LCS graph
    | null as || null bs -> return []
    | otherwise -> do
      (Snake xy uv, EditDistance d) <- middleSnake graph
      if d > 1 then do
        let (before, _) = divideGraph graph xy
        let (start, after) = divideGraph graph uv
        let (EditGraph mid _, _) = divideGraph start xy
        before' <- lcs before
        after' <- lcs after
        return $! before' <> toList mid <> after'
      else if length bs > length as then
        return (toList as)
      else
        return (toList bs)

  SES graph
    | null bs -> return (This <$> toList as)
    | null as -> return (That <$> toList bs)
    | otherwise -> do
      return []

  MiddleSnake graph -> fmap (fromMaybe (error "bleah")) $
    for [0..maxD] $ \ d ->
      (<|>)
      <$> for [negate d, negate d + 2 .. d] (\ k -> do
        forwardEndpoint <- findDPath graph Forward (EditDistance d) (Diagonal k)
        backwardV <- gets backward
        let reverseEndpoint = backwardV `at` k
        if x forwardEndpoint >= n && y forwardEndpoint >= m then
          return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance 0))
        else if odd delta && k `inInterval` (delta - pred d, delta + pred d) && overlaps forwardEndpoint reverseEndpoint then
          return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d - 1))
        else
          continue)
      <*> for [negate d, negate d + 2 .. d] (\ k -> do
        reverseEndpoint <- findDPath graph Reverse (EditDistance d) (Diagonal (k + delta))
        forwardV <- gets forward
        let forwardEndpoint = forwardV `at` (k + delta)
        if x reverseEndpoint <= 0 && y forwardEndpoint <= 0 then
          return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d))
        else if even delta && k `inInterval` (negate d, d) && overlaps forwardEndpoint reverseEndpoint then
          return (Just (Snake reverseEndpoint forwardEndpoint, EditDistance $ 2 * d))
        else
          continue)

  FindDPath _ Forward (EditDistance d) (Diagonal k) -> do
    v <- gets forward
    eq <- getEq
    let prev = v `at` pred k
    let next = v `at` succ k
    let xy = if k == negate d || k /= d && x prev < x next
          then next
          else let x' = succ (x prev) in Endpoint x' (x' - k)
    let Endpoint x' y' = slide eq xy
    setForward (v Vector.// [(maxD + k, x')])
    return (Endpoint x' y')

  FindDPath _ Reverse (EditDistance d) (Diagonal k) -> do
    v <- gets backward
    eq <- getEq
    return (Endpoint 0 0)

  where (!) = (Vector.!)
        EditGraph as bs = editGraph myers
        n = length as
        m = length bs
        delta = n - m
        maxD = (m + n) `ceilDiv` 2

        at v k = let x = v ! maxD + k in Endpoint x (x - k)

        slide eq (Endpoint x y)
          | x > 0, x < length as
          , y > 0, y < length bs
          , (as ! x) `eq` (bs ! y) = slide eq (Endpoint (succ x) (succ y))
          | otherwise = Endpoint x y


-- Smart constructors

lcs :: HasCallStack => EditGraph a -> Myers a [a]
lcs graph = M (LCS graph) `Then` return

findDPath :: HasCallStack => EditGraph a -> Direction -> EditDistance -> Diagonal -> Myers a Endpoint
findDPath graph direction d k = M (FindDPath graph direction d k) `Then` return

middleSnake :: HasCallStack => EditGraph a -> Myers a (Snake, EditDistance)
middleSnake graph = M (MiddleSnake graph) `Then` return

getEq :: HasCallStack => Myers a (a -> a -> Bool)
getEq = GetEq `Then` return


-- Implementation details

data MyersState = MyersState { forward :: !(Vector.Vector Int), backward :: !(Vector.Vector Int) }

setForward :: Vector.Vector Int -> Myers a ()
setForward v = modify (\ s -> s { forward = v })

setBackward :: Vector.Vector Int -> Myers a ()
setBackward v = modify (\ s -> s { backward = v })

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
