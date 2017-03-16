{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
module SES.Myers where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.String
import Data.These
import qualified Data.Vector as Vector
import GHC.Show
import GHC.Stack
import Prologue hiding (for, State)
import Text.Show

data MyersF a b result where
  SES :: EditGraph a b -> MyersF a b (EditScript a b)
  LCS :: EditGraph a b -> MyersF a b [(a, b)]
  EditDistance :: EditGraph a b -> MyersF a b Int
  MiddleSnake :: EditGraph a b -> MyersF a b (Snake, Distance)
  SearchUpToD :: EditGraph a b -> Distance -> MyersF a b (Maybe (Snake, Distance))
  SearchAlongK :: EditGraph a b -> Distance -> Direction -> Diagonal -> MyersF a b (Maybe (Snake, Distance))
  FindDPath :: EditGraph a b -> Distance -> Direction -> Diagonal -> MyersF a b Endpoint

  GetK :: EditGraph a b -> Direction -> Diagonal -> MyersF a b (Int, EditScript a b)
  SetK :: EditGraph a b -> Direction -> Diagonal -> Int -> EditScript a b -> MyersF a b ()

  Slide :: EditGraph a b -> Direction -> Endpoint -> EditScript a b -> MyersF a b (Endpoint, EditScript a b)

type EditScript a b = [These a b]

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF a b result where
  M :: MyersF a b c -> StepF a b c
  S :: State (MyersState a b) c -> StepF a b c
  GetEq :: StepF a b (a -> b -> Bool)

type Myers a b = Freer (StepF a b)

data EditGraph a b = EditGraph { as :: !(Vector.Vector a), bs :: !(Vector.Vector b) }
  deriving (Eq, Show)

data Snake = Snake { xy :: Endpoint, uv :: Endpoint }
  deriving (Eq, Show)

newtype Distance = Distance { unDistance :: Int }
  deriving (Eq, Show)

newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Show)

data Endpoint = Endpoint { x :: !Int, y :: !Int }
  deriving (Eq, Show)

data Direction = Forward | Reverse
  deriving (Eq, Show)


-- Evaluation

runMyers :: forall a b c. HasCallStack => (a -> b -> Bool) -> Myers a b c -> c
runMyers eq step = evalState (go step) (emptyStateForStep step)
  where go :: forall c. Myers a b c -> StateT (MyersState a b) Identity c
        go = iterFreerA algebra
        algebra :: forall c x. StepF a b x -> (x -> StateT (MyersState a b) Identity c) -> StateT (MyersState a b) Identity c
        algebra step cont = case step of
          M m -> go (decompose m) >>= cont
          S Get -> get >>= cont
          S (Put s) -> put s >>= cont
          GetEq -> cont eq

runMyersSteps :: HasCallStack => (a -> b -> Bool) -> Myers a b c -> [(MyersState a b, Myers a b c)]
runMyersSteps eq step = go (emptyStateForStep step) step
  where go state step = let ?callStack = popCallStack callStack in prefix state step $ case runMyersStep eq state step of
          Left result -> [ (state, return result) ]
          Right next -> uncurry go next
        prefix state step = case step of
          Then (M _) _ -> ((state, step) :)
          _ -> identity

runMyersStep :: HasCallStack => (a -> b -> Bool) -> MyersState a b -> Myers a b c -> Either c (MyersState a b, Myers a b c)
runMyersStep eq state step = let ?callStack = popCallStack callStack in case step of
  Return a -> Left a
  Then step cont -> case step of
    M myers -> Right (state, decompose myers >>= cont)

    S Get -> Right (state, cont state)
    S (Put state') -> Right (state', cont ())

    GetEq -> Right (state, cont eq)


decompose :: HasCallStack => MyersF a b c -> Myers a b c
decompose myers = let ?callStack = popCallStack callStack in case myers of
  LCS graph
    | null as || null bs -> return []
    | otherwise -> do
      result <- ses graph
      return (catMaybes (these (const Nothing) (const Nothing) ((Just .) . (,)) <$> result))

  SES graph
    | null bs -> return (This <$> toList as)
    | null as -> return (That <$> toList bs)
    | otherwise -> do
      Just (_, Distance d) <- for [0..maxD] (searchUpToD graph . Distance)
      v <- gets ((if odd d then fst else snd) . unMyersState)
      return (snd (v Vector.! if odd d then 0 else index v delta))

  EditDistance graph -> unDistance . snd <$> middleSnake graph

  MiddleSnake graph -> do
    Just result <- for [0..maxD] (searchUpToD graph . Distance)
    return result

  SearchUpToD graph (Distance d) ->
    (<|>) <$> for [negate d, negate d + 2 .. d] (searchAlongK graph (Distance d) Forward . Diagonal)
          <*> for [negate d, negate d + 2 .. d] (searchAlongK graph (Distance d) Reverse . Diagonal)

  SearchAlongK graph d direction k -> do
    (forwardEndpoint, reverseEndpoint) <- endpointsFor graph d direction (diagonalFor direction k)
    if shouldTestOn direction && inInterval d direction k && overlaps graph forwardEndpoint reverseEndpoint then
      return (Just (Snake reverseEndpoint forwardEndpoint, editDistance direction d))
    else
      continue

  FindDPath graph (Distance d) direction (Diagonal k) -> do
    (prev, prevScript) <- getK graph direction (Diagonal (pred k))
    (next, nextScript) <- getK graph direction (Diagonal (succ k))
    let (fromX, fromScript) = if k == negate d || k /= d && prev < next
          then (next,      addInBounds bs (next - succ k) That nextScript) -- downward (insertion)
          else (succ prev, addInBounds as  prev           This prevScript) -- rightward (deletion)
    (endpoint, script) <- slide graph direction (Endpoint fromX (fromX - k)) fromScript
    setK graph direction (Diagonal k) (x endpoint) script
    return $ case direction of
      Forward -> endpoint
      Reverse -> Endpoint (n - x endpoint) (m - y endpoint)
    where at :: Vector.Vector a -> Int -> a
          v `at` i = v Vector.! case direction of { Forward -> i ; Reverse -> length v - succ i }
          addInBounds :: Vector.Vector a -> Int -> (a -> b) -> [b] -> [b]
          addInBounds v i with to = if (d /= 0 || direction == Reverse) && i >= 0 && i < length v then addFor direction (with (v `at` i)) to else to

  GetK _ direction (Diagonal k) -> do
    v <- gets (stateFor direction)
    return (v Vector.! index v k)

  SetK _ direction (Diagonal k) x script ->
    setStateFor direction (\ v -> v Vector.// [(index v k, (x, script))])

  Slide graph direction (Endpoint x y) script
    | x >= 0, x < n
    , y >= 0, y < m -> do
      eq <- getEq
      let a = as `at` x
      let b = bs `at` y
      if a `eq` b
        then slide graph direction (Endpoint (succ x) (succ y)) (addFor direction (These a b) script)
        else return (Endpoint x y, script)
    | otherwise -> return (Endpoint x y, script)
    where at :: Vector.Vector a -> Int -> a
          v `at` i = v Vector.! case direction of { Forward -> i ; Reverse -> length v - succ i }

  where (EditGraph as bs, n, m, maxD, delta) = editGraph myers

        index v k = if k >= 0 then k else length v + k

        inInterval (Distance d) direction (Diagonal k) = case direction of
          Forward -> k >= (delta - pred d) && k <= (delta + pred d)
          Reverse -> (k + delta) >= negate d && (k + delta) <= d

        diagonalFor Forward k = k
        diagonalFor Reverse (Diagonal k) = Diagonal (k + delta)

        shouldTestOn Forward = odd delta
        shouldTestOn Reverse = even delta

        stateFor Forward = fst . unMyersState
        stateFor Reverse = snd . unMyersState

        setStateFor Forward f = modify (MyersState . first f . unMyersState)
        setStateFor Reverse f = modify (MyersState . second f . unMyersState)

        addFor :: Direction -> a -> [a] -> [a]
        addFor dir a = case dir of { Forward -> (<> [a]) ; Reverse -> (a :) }

        endpointsFor graph d direction k = do
          here <- findDPath graph d direction k
          (x, _) <- getK graph (invert direction) k
          let there = Endpoint x (x - unDiagonal k)
          case direction of
            Forward -> return (here, there)
            Reverse -> return (there, here)

        invert Forward = Reverse
        invert Reverse = Forward

        editDistance Forward (Distance d) = Distance (2 * d - 1)
        editDistance Reverse (Distance d) = Distance (2 * d)


-- Smart constructors

ses :: HasCallStack => EditGraph a b -> Myers a b (EditScript a b)
ses graph = M (SES graph) `Then` return

lcs :: HasCallStack => EditGraph a b -> Myers a b [(a, b)]
lcs graph = M (LCS graph) `Then` return

editDistance :: HasCallStack => EditGraph a b -> Myers a b Int
editDistance graph = M (EditDistance graph) `Then` return

middleSnake :: HasCallStack => EditGraph a b -> Myers a b (Snake, Distance)
middleSnake graph = M (MiddleSnake graph) `Then` return

searchUpToD :: HasCallStack => EditGraph a b -> Distance -> Myers a b (Maybe (Snake, Distance))
searchUpToD graph distance = M (SearchUpToD graph distance) `Then` return

searchAlongK :: HasCallStack => EditGraph a b -> Distance -> Direction -> Diagonal -> Myers a b (Maybe (Snake, Distance))
searchAlongK graph d direction k = M (SearchAlongK graph d direction k) `Then` return

findDPath :: HasCallStack => EditGraph a b -> Distance -> Direction -> Diagonal -> Myers a b Endpoint
findDPath graph d direction k = M (FindDPath graph d direction k) `Then` return

getK :: HasCallStack => EditGraph a b -> Direction -> Diagonal -> Myers a b (Int, EditScript a b)
getK graph direction diagonal = M (GetK graph direction diagonal) `Then` return

setK :: HasCallStack => EditGraph a b -> Direction -> Diagonal -> Int -> EditScript a b -> Myers a b ()
setK graph direction diagonal x script = M (SetK graph direction diagonal x script) `Then` return

slide :: HasCallStack => EditGraph a b -> Direction -> Endpoint -> EditScript a b -> Myers a b (Endpoint, EditScript a b)
slide graph direction from script = M (Slide graph direction from script) `Then` return

getEq :: HasCallStack => Myers a b (a -> b -> Bool)
getEq = GetEq `Then` return


-- Implementation details

newtype MyersState a b = MyersState { unMyersState :: (Vector.Vector (Int, (EditScript a b)), Vector.Vector (Int, (EditScript a b))) }
  deriving (Eq, Show)

emptyStateForStep :: Myers a b c -> MyersState a b
emptyStateForStep step = case step of
  Then (M myers) _ ->
    let (_, _, _, maxD, _) = editGraph myers in
    MyersState (Vector.replicate (succ (maxD * 2)) (0, []), Vector.replicate (succ (maxD * 2)) (0, []))
  _ -> MyersState (Vector.empty, Vector.empty)

overlaps :: EditGraph a b -> Endpoint -> Endpoint -> Bool
overlaps (EditGraph as _) (Endpoint x y) (Endpoint u v) = x - y == u - v && length as - u <= x

for :: [a] -> (a -> Myers c d (Maybe b)) -> Myers c d (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

continue :: Myers b c (Maybe a)
continue = return Nothing

ceilDiv :: Integral a => a -> a -> a
ceilDiv = (uncurry (+) .) . divMod

divideGraph :: EditGraph a b -> Endpoint -> (EditGraph a b, EditGraph a b)
divideGraph (EditGraph as bs) (Endpoint x y) =
  ( EditGraph (slice 0  x              as) (slice 0  y              bs)
  , EditGraph (slice x (length as - x) as) (slice y (length bs - y) bs) )
  where slice from to v = Vector.slice (max 0 (min from (length v))) (max 0 (min to (length v))) v


editGraph :: MyersF a b c -> (EditGraph a b, Int, Int, Int, Int)
editGraph myers = (EditGraph as bs, n, m, (m + n) `ceilDiv` 2, n - m)
  where EditGraph as bs = case myers of
          SES g -> g
          LCS g -> g
          EditDistance g -> g
          MiddleSnake g -> g
          SearchUpToD g _ -> g
          SearchAlongK g _ _ _ -> g
          FindDPath g _ _ _ -> g
          GetK g _ _ -> g
          SetK g _ _ _ _ -> g
          Slide g _ _ _ -> g
        (n, m) = (length as, length bs)


liftShowsVector :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Vector.Vector a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList

liftShowsMyersF :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> MyersF a b c -> ShowS
liftShowsMyersF sp1 sl1 sp2 sl2 d m = case m of
  SES graph -> showsUnaryWith showGraph "SES" d graph
  LCS graph -> showsUnaryWith showGraph "LCS" d graph
  EditDistance graph -> showsUnaryWith showGraph "EditDistance" d graph
  MiddleSnake graph -> showsUnaryWith showGraph "MiddleSnake" d graph
  SearchUpToD graph distance -> showsBinaryWith showGraph showsPrec "SearchUpToD" d graph distance
  SearchAlongK graph distance direction diagonal -> showsQuaternaryWith showGraph showsPrec showsPrec showsPrec "SearchAlongK" d graph direction distance diagonal
  FindDPath graph distance direction diagonal -> showsQuaternaryWith showGraph showsPrec showsPrec showsPrec "FindDPath" d graph distance direction diagonal
  GetK graph direction diagonal -> showsTernaryWith showGraph showsPrec showsPrec "GetK" d graph direction diagonal
  SetK graph direction diagonal v script -> showsQuinaryWith showGraph showsPrec showsPrec showsPrec (liftShowsEditScript sp1 sp2) "SetK" d graph direction diagonal v script
  Slide graph direction endpoint script -> showsQuaternaryWith showGraph showsPrec showsPrec (liftShowsEditScript sp1 sp2) "Slide" d graph direction endpoint script
  where showGraph = (liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> EditGraph a b -> ShowS) sp1 sl1 sp2 sl2

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

showsQuaternaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> (Int -> d -> ShowS) -> String -> Int -> a -> b -> c -> d -> ShowS
showsQuaternaryWith sp1 sp2 sp3 sp4 name d x y z w = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z . showChar ' ' . sp4 11 w

showsQuinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> (Int -> d -> ShowS) -> (Int -> e -> ShowS) -> String -> Int -> a -> b -> c -> d -> e -> ShowS
showsQuinaryWith sp1 sp2 sp3 sp4 sp5 name d x y z w v = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z . showChar ' ' . sp4 11 w . showChar ' ' . sp5 11 v

liftShowsState :: (Int -> a -> ShowS) -> Int -> State a b -> ShowS
liftShowsState sp d state = case state of
  Get -> showString "Get"
  Put s -> showsUnaryWith sp "Put" d s

liftShowsStepF :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> StepF a b c -> ShowS
liftShowsStepF sp1 sl1 sp2 sl2 d step = case step of
  M m -> showsUnaryWith (liftShowsMyersF sp1 sl1 sp2 sl2) "M" d m
  S s -> showsUnaryWith (liftShowsState (liftShowsPrec2 sp1 sl1 sp2 sl2)) "S" d s
  GetEq -> showString "GetEq"

liftShowsThese :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> These a b -> ShowS
liftShowsThese sa sb d t = case t of
  This a -> showsUnaryWith sa "This" d a
  That b -> showsUnaryWith sb "That" d b
  These a b -> showsBinaryWith sa sb "These" d a b

liftShowsEditScript :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> EditScript a b -> ShowS
liftShowsEditScript sa sb _ = showListWith (liftShowsThese sa sb 0)


-- Instances

instance MonadState (MyersState a b) (Myers a b) where
  get = S Get `Then` return
  put a = S (Put a) `Then` return

instance Show2 MyersState where
  liftShowsPrec2 sp1 _ sp2 _ d (MyersState (v1, v2)) = showsUnaryWith (showsWith (showsWith liftShowsPrec2 showsStateVector) showsStateVector) "MyersState" d (v1, v2)
    where showsStateVector = showsWith liftShowsVector (showsWith liftShowsPrec (liftShowsEditScript sp1 sp2))
          showsWith g f = g f (showListWith (f 0))

instance Show s => Show1 (State s) where
  liftShowsPrec _ _ = liftShowsState showsPrec

instance Show s => Show (State s a) where
  showsPrec = liftShowsPrec (const (const identity)) (const identity)

instance Show2 EditGraph where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (EditGraph as bs) = showsBinaryWith (liftShowsVector sp1 sl1) (liftShowsVector sp2 sl2) "EditGraph" d as bs

instance (Show a, Show b) => Show1 (MyersF a b) where
  liftShowsPrec _ _ = liftShowsMyersF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show (MyersF a b c) where
  showsPrec = liftShowsMyersF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show1 (StepF a b) where
  liftShowsPrec _ _ = liftShowsStepF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show (StepF a b c) where
  showsPrec = liftShowsStepF showsPrec showList showsPrec showList
