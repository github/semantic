{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
module SES.Myers where

import Control.Exception
import Control.Monad.Free.Freer
import qualified Data.Array as Array
import Data.Ix (inRange)
import Data.Functor.Classes
import Data.String
import Data.These
import GHC.Show hiding (show)
import GHC.Stack
import Prologue hiding (for, State, error)
import Text.Show (showListWith)

data MyersF a b result where
  SES :: MyersF a b (EditScript a b)
  LCS :: MyersF a b [(a, b)]
  EditDistance :: MyersF a b Int
  SearchUpToD :: Distance -> MyersF a b (Maybe (EditScript a b, Distance))
  SearchAlongK :: Distance -> Diagonal -> MyersF a b (Maybe (EditScript a b, Distance))
  MoveFromAdjacent :: Distance -> Diagonal -> MyersF a b (Endpoint a b)
  MoveDownFrom :: Endpoint a b -> MyersF a b (Endpoint a b)
  MoveRightFrom :: Endpoint a b -> MyersF a b (Endpoint a b)

  GetK :: Diagonal -> MyersF a b (Endpoint a b)
  SetK :: Diagonal -> Endpoint a b -> MyersF a b ()

  Slide :: Endpoint a b -> MyersF a b (Endpoint a b)

type EditScript a b = [These a b]

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF a b result where
  M :: HasCallStack => MyersF a b c -> StepF a b c
  S :: State (MyersState a b) c -> StepF a b c

type Myers a b = Freer (StepF a b)

data EditGraph a b = EditGraph { as :: !(Array.Array Int a), bs :: !(Array.Array Int b) }
  deriving (Eq, Show)

makeEditGraph :: (Foldable t, Foldable u) => t a -> u b -> EditGraph a b
makeEditGraph as bs = EditGraph (Array.listArray (0, pred (length as)) (toList as)) (Array.listArray (0, pred (length bs)) (toList bs))

newtype Distance = Distance { unDistance :: Int }
  deriving (Eq, Show)

newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Show)

data Endpoint a b = Endpoint { x :: !Int, y :: !Int, script :: !(EditScript a b) }
  deriving (Eq, Show)


-- API

ses :: (HasCallStack, Foldable t, Foldable u) => (a -> b -> Bool) -> t a -> u b -> EditScript a b
ses eq as bs = runMyers eq (makeEditGraph as bs) (M SES `Then` return)


-- Evaluation

runMyers :: forall a b c. HasCallStack => (a -> b -> Bool) -> EditGraph a b ->Myers a b c -> c
runMyers eq graph step = evalState (go step) (emptyStateForGraph graph)
  where go :: forall c. Myers a b c -> StateT (MyersState a b) Identity c
        go = iterFreerA algebra
        algebra :: forall c x. StepF a b x -> (x -> StateT (MyersState a b) Identity c) -> StateT (MyersState a b) Identity c
        algebra step cont = case step of
          M m -> go (decompose eq graph m) >>= cont
          S Get -> get >>= cont
          S (Put s) -> put s >>= cont

runMyersSteps :: HasCallStack => (a -> b -> Bool) -> EditGraph a b ->Myers a b c -> [(MyersState a b, Myers a b c)]
runMyersSteps eq graph = go (emptyStateForGraph graph)
  where go state step = let ?callStack = popCallStack callStack in prefix state step $ case runMyersStep eq graph state step of
          Left result -> [ (state, return result) ]
          Right next -> uncurry go next
        prefix state step = case step of
          Then (M _) _ -> ((state, step) :)
          _ -> identity

runMyersStep :: HasCallStack => (a -> b -> Bool) -> EditGraph a b ->MyersState a b -> Myers a b c -> Either c (MyersState a b, Myers a b c)
runMyersStep eq graph state step = let ?callStack = popCallStack callStack in case step of
  Return a -> Left a
  Then step cont -> case step of
    M myers -> Right (state, decompose eq graph myers >>= cont)

    S Get -> Right (state, cont state)
    S (Put state') -> Right (state', cont ())


decompose :: HasCallStack => (a -> b -> Bool) -> EditGraph a b ->MyersF a b c -> Myers a b c
decompose eq graph myers = let ?callStack = popCallStack callStack in case myers of
  SES -> runSES graph
  LCS -> runLCS graph
  EditDistance -> runEditDistance graph
  SearchUpToD d -> runSearchUpToD graph d
  SearchAlongK d k -> runSearchAlongK graph d k
  MoveFromAdjacent d k -> runMoveFromAdjacent graph d k
  MoveDownFrom e -> runMoveDownFrom graph e
  MoveRightFrom e -> runMoveRightFrom graph e

  GetK k -> runGetK graph k
  SetK k x -> runSetK graph k x

  Slide from -> runSlide eq graph from
{-# INLINE decompose #-}


runSES :: HasCallStack => EditGraph a b -> Myers a b (EditScript a b)
runSES (EditGraph as bs)
  | null bs = return (This <$> toList as)
  | null as = return (That <$> toList bs)
  | otherwise = let ?callStack = popCallStack callStack in do
    result <- for [0..(length as + length bs)] (searchUpToD . Distance)
    case result of
      Just (script, _) -> return (reverse script)
      _ -> fail "no shortest edit script found in edit graph (this is a bug in SES.Myers)."

runLCS :: HasCallStack => EditGraph a b -> Myers a b [(a, b)]
runLCS (EditGraph as bs)
  | null as || null bs = return []
  | otherwise = let ?callStack = popCallStack callStack in do
    result <- M SES `Then` return
    return (catMaybes (these (const Nothing) (const Nothing) ((Just .) . (,)) <$> result))

runEditDistance :: HasCallStack => EditGraph a b -> Myers a b Int
runEditDistance _ = let ?callStack = popCallStack callStack in length . filter (these (const True) (const True) (const (const False))) <$> (M SES `Then` return)


runSearchUpToD :: HasCallStack => EditGraph a b -> Distance -> Myers a b (Maybe (EditScript a b, Distance))
runSearchUpToD (EditGraph as bs) (Distance d) = let ?callStack = popCallStack callStack in
  for [ k | k <- [negate d, negate d + 2 .. d], inRange (negate m, n) k ] (searchAlongK (Distance d) . Diagonal)
  where (n, m) = (length as, length bs)

runSearchAlongK :: HasCallStack => EditGraph a b -> Distance -> Diagonal -> Myers a b (Maybe (EditScript a b, Distance))
runSearchAlongK (EditGraph as bs) d k = let ?callStack = popCallStack callStack in do
  Endpoint x y script <- moveFromAdjacent d k
  if x >= length as && y >= length bs then
    return (Just (script, d))
  else
    continue

runMoveFromAdjacent :: HasCallStack => EditGraph a b -> Distance -> Diagonal -> Myers a b (Endpoint a b)
runMoveFromAdjacent (EditGraph as bs) (Distance d) (Diagonal k) = let ?callStack = popCallStack callStack in do
  let (n, m) = (length as, length bs)
  from <- if d == 0 || k < negate m || k > n then
    return (Endpoint 0 0 [])
  else if k == negate d || k == negate m then
    getK (Diagonal (succ k)) >>= moveDownFrom
  else if k /= d && k /= n then do
    prev <- getK (Diagonal (pred k))
    next <- getK (Diagonal (succ k))
    if x prev < x next then
      moveDownFrom next
    else
      moveRightFrom prev
  else
    getK (Diagonal (pred k)) >>= moveRightFrom
  endpoint <- slide from
  setK (Diagonal k) endpoint
  return endpoint

runMoveDownFrom :: HasCallStack => EditGraph a b -> Endpoint a b -> Myers a b (Endpoint a b)
runMoveDownFrom (EditGraph _ bs) (Endpoint x y script) = return (Endpoint x (succ y) (if y < length bs then That (bs ! y) : script else script))

runMoveRightFrom :: HasCallStack => EditGraph a b -> Endpoint a b -> Myers a b (Endpoint a b)
runMoveRightFrom (EditGraph as _) (Endpoint x y script) = return (Endpoint (succ x) y (if x < length as then This (as ! x) : script else script))

runGetK :: HasCallStack => EditGraph a b -> Diagonal -> Myers a b (Endpoint a b)
runGetK graph k = let ?callStack = popCallStack callStack in do
  (i, v) <- checkK graph k
  let (x, script) = v ! i in return (Endpoint x (x - unDiagonal k) script)

runSetK :: HasCallStack => EditGraph a b -> Diagonal -> Endpoint a b -> Myers a b ()
runSetK graph k (Endpoint x _ script) = let ?callStack = popCallStack callStack in do
  (i, v) <- checkK graph k
  put (MyersState (v Array.// [(i, (x, script))]))

runSlide :: HasCallStack => (a -> b -> Bool) -> EditGraph a b -> Endpoint a b -> Myers a b (Endpoint a b)
runSlide eq (EditGraph as bs) (Endpoint x y script)
  | x >= 0, x < length as
  , y >= 0, y < length bs
  , a <- as ! x
  , b <- bs ! y
  , a `eq` b  = slide  (Endpoint (succ x) (succ y) (These a b : script))
  | otherwise = return (Endpoint       x        y               script)


-- Smart constructors

lcs :: HasCallStack => Myers a b [(a, b)]
lcs = M LCS `Then` return

editDistance :: HasCallStack => Myers a b Int
editDistance = M EditDistance `Then` return

searchUpToD :: HasCallStack => Distance -> Myers a b (Maybe (EditScript a b, Distance))
searchUpToD distance = M (SearchUpToD distance) `Then` return

searchAlongK :: HasCallStack => Distance -> Diagonal -> Myers a b (Maybe (EditScript a b, Distance))
searchAlongK d k = M (SearchAlongK d k) `Then` return

moveFromAdjacent :: HasCallStack => Distance -> Diagonal -> Myers a b (Endpoint a b)
moveFromAdjacent d k = M (MoveFromAdjacent d k) `Then` return

moveDownFrom :: HasCallStack => Endpoint a b -> Myers a b (Endpoint a b)
moveDownFrom e = M (MoveDownFrom e) `Then` return

moveRightFrom :: HasCallStack => Endpoint a b -> Myers a b (Endpoint a b)
moveRightFrom e = M (MoveRightFrom e) `Then` return

getK :: HasCallStack => Diagonal -> Myers a b (Endpoint a b)
getK diagonal = M (GetK diagonal) `Then` return

setK :: HasCallStack => Diagonal -> Endpoint a b -> Myers a b ()
setK diagonal x = M (SetK diagonal x) `Then` return

slide :: HasCallStack => Endpoint a b -> Myers a b (Endpoint a b)
slide from = M (Slide from) `Then` return


-- Implementation details

newtype MyersState a b = MyersState { unMyersState :: Array.Array Int (Int, EditScript a b) }
  deriving (Eq, Show)

emptyStateForGraph :: EditGraph a b -> MyersState a b
emptyStateForGraph (EditGraph as bs) = let (n, m) = (length as, length bs) in
  MyersState (Array.listArray (0, m + n) (repeat (0, [])))

for :: [a] -> (a -> Myers c d (Maybe b)) -> Myers c d (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

continue :: Myers b c (Maybe a)
continue = return Nothing

index :: Array.Array Int a -> Int -> Int
index v k = if k >= 0 then k else length v + k


fail :: (HasCallStack, Monad m) => String -> m a
fail s = let ?callStack = fromCallSiteList (filter ((/= "M") . fst) (getCallStack callStack)) in
  throw (MyersException s callStack)

(!) :: HasCallStack => Array.Array Int a -> Int -> a
v ! i | i < length v = v Array.! i
      | otherwise = let ?callStack = fromCallSiteList (filter ((/= "M") . fst) (getCallStack callStack)) in
          throw (MyersException ("index " <> show i <> " out of bounds") callStack)

checkK :: HasCallStack => EditGraph a b -> Diagonal -> Myers a b (Int, Array.Array Int (Int, EditScript a b))
checkK (EditGraph as bs) (Diagonal k) = let ?callStack = popCallStack callStack in do
  v <- gets unMyersState
  let i = index v k
  let (n, m) = (length as, length bs)
  when (i < 0) $
    fail ("diagonal " <> show k <> " (" <> show i <> ") underflows state indices " <> show (negate m) <> ".." <> show n <> " (0.." <> show (succ (m + n)) <> ")")
  when (i >= length v) $
    fail ("diagonal " <> show k <> " (" <> show i <> ") overflows state indices " <> show (negate m) <> ".." <> show n <> " (0.." <> show (succ (m + n)) <> ")")
  return (i, v)


liftShowsVector :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array.Array Int a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList

liftShowsMyersF :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> MyersF a b c -> ShowS
liftShowsMyersF sp1 sp2 d m = case m of
  SES -> showString "SES"
  LCS -> showString "LCS"
  EditDistance -> showString "EditDistance"
  SearchUpToD distance -> showsUnaryWith showsPrec "SearchUpToD" d distance
  SearchAlongK distance diagonal -> showsBinaryWith showsPrec showsPrec "SearchAlongK" d distance diagonal
  MoveFromAdjacent distance diagonal -> showsBinaryWith showsPrec showsPrec "MoveFromAdjacent" d distance diagonal
  MoveDownFrom endpoint -> showsUnaryWith (liftShowsEndpoint sp1 sp2) "MoveDownFrom" d endpoint
  MoveRightFrom endpoint -> showsUnaryWith (liftShowsEndpoint sp1 sp2) "MoveRightFrom" d endpoint
  GetK diagonal -> showsUnaryWith showsPrec "GetK" d diagonal
  SetK diagonal v -> showsBinaryWith showsPrec (liftShowsEndpoint sp1 sp2) "SetK" d diagonal v
  Slide endpoint -> showsUnaryWith (liftShowsEndpoint sp1 sp2) "Slide" d endpoint

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
  M m -> showsUnaryWith (liftShowsMyersF sp1 sp2) "M" d m
  S s -> showsUnaryWith (liftShowsState (liftShowsPrec2 sp1 sl1 sp2 sl2)) "S" d s

liftShowsThese :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> These a b -> ShowS
liftShowsThese sa sb d t = case t of
  This a -> showsUnaryWith sa "This" d a
  That b -> showsUnaryWith sb "That" d b
  These a b -> showsBinaryWith sa sb "These" d a b

liftShowsEditScript :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> EditScript a b -> ShowS
liftShowsEditScript sa sb _ = showListWith (liftShowsThese sa sb 0)

liftShowsEndpoint :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> Endpoint a b -> ShowS
liftShowsEndpoint sp1 sp2 d (Endpoint x y script) = showsTernaryWith showsPrec showsPrec (liftShowsEditScript sp1 sp2) "Endpoint" d x y script

data MyersException = MyersException String CallStack
  deriving (Typeable)


-- Instances

instance MonadState (MyersState a b) (Myers a b) where
  get = S Get `Then` return
  put a = S (Put a) `Then` return

instance Show2 MyersState where
  liftShowsPrec2 sp1 _ sp2 _ d (MyersState v) = showsUnaryWith showsStateVector "MyersState" d v
    where showsStateVector = showsWith liftShowsVector (showsWith liftShowsPrec (liftShowsEditScript sp1 sp2))
          showsWith g f = g f (showListWith (f 0))

instance Show s => Show1 (State s) where
  liftShowsPrec _ _ = liftShowsState showsPrec

instance Show s => Show (State s a) where
  showsPrec = liftShowsPrec (const (const identity)) (const identity)

instance Show2 EditGraph where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (EditGraph as bs) = showsBinaryWith (liftShowsVector sp1 sl1) (liftShowsVector sp2 sl2) "EditGraph" d as bs

instance Show2 Endpoint where
  liftShowsPrec2 sp1 _ sp2 _ = liftShowsEndpoint sp1 sp2

instance (Show a, Show b) => Show1 (MyersF a b) where
  liftShowsPrec _ _ = liftShowsMyersF showsPrec showsPrec

instance (Show a, Show b) => Show (MyersF a b c) where
  showsPrec = liftShowsMyersF showsPrec showsPrec

instance (Show a, Show b) => Show1 (StepF a b) where
  liftShowsPrec _ _ = liftShowsStepF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show (StepF a b c) where
  showsPrec = liftShowsStepF showsPrec showList showsPrec showList

instance Exception MyersException

instance Show MyersException where
  showsPrec _ (MyersException s c) = showString "Exception: " . showString s . showChar '\n' . showString (prettyCallStack c)
