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
  deriving (Eq, Show)

newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Show)

data Endpoint = Endpoint { x :: !Int, y :: !Int }
  deriving (Eq, Show)

data Direction = Forward | Reverse
  deriving (Eq, Show)


-- Evaluation

runMyers :: HasCallStack => (a -> a -> Bool) -> Myers a b -> b
runMyers eq = runAll emptyState
  where runAll state step = case runMyersStep eq state step of
          Left a -> a
          Right next -> uncurry runAll next

runMyersSteps :: HasCallStack => (a -> a -> Bool) -> MyersState -> Myers a b -> [(MyersState, Myers a b)]
runMyersSteps eq state step = let ?callStack = popCallStack callStack in (state, step) : case runMyersStep eq state step of
  Left result -> [ (state, return result) ]
  Right next -> uncurry (runMyersSteps eq) next

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
      (Snake xy uv, EditDistance d) <- middleSnake graph
      if d > 1 then do
        let (before, _) = divideGraph graph xy
        let (start, after) = divideGraph graph uv
        let (EditGraph midAs midBs, _) = divideGraph start xy
        before' <- ses before
        after' <- ses after
        return $! before' <> zipWith These (toList midAs) (toList midBs) <> after'
      else
        return (zipWith These (toList as) (toList bs))

  MiddleSnake graph -> fmap (fromMaybe (error "bleah")) $
    for [0..maxD] $ \ d ->
      (<|>)
      <$> for [negate d, negate d + 2 .. d] (\ k -> do
        forwardEndpoint <- findDPath graph Forward (EditDistance d) (Diagonal k)
        backwardV <- gets backward
        let reverseEndpoint = let x = backwardV `at` k in Endpoint x (x - k)
        if odd delta && k `inInterval` (delta - pred d, delta + pred d) && overlaps graph forwardEndpoint reverseEndpoint then
          return (Just (Snake (Endpoint (n - x reverseEndpoint) (m - y reverseEndpoint)) forwardEndpoint, EditDistance $ 2 * d - 1))
        else
          continue)
      <*> for [negate d, negate d + 2 .. d] (\ k -> do
        reverseEndpoint <- findDPath graph Reverse (EditDistance d) (Diagonal (k + delta))
        forwardV <- gets forward
        let forwardEndpoint = let x = forwardV `at` (k + delta) in Endpoint x (x - k)
        if even delta && (k + delta) `inInterval` (negate d, d) && overlaps graph forwardEndpoint reverseEndpoint then
          return (Just (Snake (Endpoint (n - x reverseEndpoint) (m - y reverseEndpoint)) forwardEndpoint, EditDistance $ 2 * d))
        else
          continue)

  FindDPath _ Forward (EditDistance d) (Diagonal k) -> do
    v <- gets forward
    eq <- getEq
    let prev = v `at` pred k
    let next = v `at` succ k
    let x = if k == negate d || k /= d && prev < next
          then next
          else succ prev
    let Endpoint x' y' = slide Reverse eq (Endpoint x (x - k))
    setForward (v Vector.// [(maxD + k, x')])
    return (Endpoint x' y')

  FindDPath _ Reverse (EditDistance d) (Diagonal k) -> do
    v <- gets backward
    eq <- getEq
    let prev = v `at` pred k
    let next = v `at` succ k
    let x = if k == negate d || k /= d && prev < next
          then next
          else succ prev
    let Endpoint x' y' = slide Reverse eq (Endpoint x (x - k))
    setBackward (v Vector.// [(maxD + k, x')])
    return (Endpoint x' y')

  where (!) = (Vector.!)
        EditGraph as bs = editGraph myers
        n = length as
        m = length bs
        delta = n - m
        maxD = (m + n) `ceilDiv` 2

        at v k = v ! maxD + k

        slide dir eq (Endpoint x y)
          | x >= 0, x < length as
          , y >= 0, y < length bs
          , nth dir as x `eq` nth dir bs y = slide dir eq (Endpoint (succ x) (succ y))
          | otherwise = Endpoint x y

        nth Forward v i = v ! i
        nth Reverse v i = v ! (length v - 1 - i)


-- Smart constructors

ses :: HasCallStack => EditGraph a -> Myers a [These a a]
ses graph = M (SES graph) `Then` return

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

emptyState :: MyersState
emptyState = MyersState (Vector.replicate 100 0) (Vector.replicate 100 0)

setForward :: Vector.Vector Int -> Myers a ()
setForward v = modify (\ s -> s { forward = v })

setBackward :: Vector.Vector Int -> Myers a ()
setBackward v = modify (\ s -> s { backward = v })

overlaps :: EditGraph a -> Endpoint -> Endpoint -> Bool
overlaps (EditGraph as _) (Endpoint x y) (Endpoint u v) = x - y == u - v && x <= length as - u

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
  ( EditGraph (slice 0  x              as) (slice 0  y              bs)
  , EditGraph (slice x (length as - x) as) (slice y (length bs - y) bs) )
  where slice from to v = Vector.slice (max 0 (min from (length v))) (max 0 (min to (length v))) v


editGraph :: MyersF a b -> EditGraph a
editGraph myers = case myers of
  SES g -> g
  LCS g -> g
  MiddleSnake g -> g
  FindDPath g _ _ _ -> g


-- Instances

instance MonadState MyersState (Myers a) where
  get = S Get `Then` return
  put a = S (Put a) `Then` return
