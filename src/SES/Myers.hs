{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
module SES.Myers
( MyersF(..)
, EditScript
, Step(..)
, Myers
, EditGraph(..)
, Distance(..)
, Diagonal(..)
, Endpoint(..)
, ses
, runMyers
, runMyersSteps
, lcs
, editDistance
, MyersState(..)
) where

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

-- | Operations in Myers’ algorithm.
data MyersF a b result where
  SES :: MyersF a b (EditScript a b)
  LCS :: MyersF a b [(a, b)]
  EditDistance :: MyersF a b Int
  SearchUpToD :: Distance -> MyersF a b (Maybe (EditScript a b, Distance))
  SearchAlongK :: Distance -> Diagonal -> MyersF a b (Maybe (EditScript a b, Distance))
  MoveFromAdjacent :: Distance -> Diagonal -> MyersF a b (Endpoint a b)
  MoveDownFrom :: Endpoint a b -> MyersF a b (Endpoint a b)
  MoveRightFrom :: Endpoint a b -> MyersF a b (Endpoint a b)
  SlideFrom :: Endpoint a b -> MyersF a b (Endpoint a b)

  GetK :: Diagonal -> MyersF a b (Endpoint a b)
  SetK :: Diagonal -> Endpoint a b -> MyersF a b ()


-- | An edit script, i.e. a sequence of changes/copies of elements.
type EditScript a b = [These a b]

-- | Steps in the execution of Myers’ algorithm, i.e. the sum of MyersF and State.
data Step a b result where
  M :: HasCallStack => MyersF a b c -> Step a b c
  S :: State (MyersState a b) c -> Step a b c

type Myers a b = Freer (Step a b)

-- | Notionally the cartesian product of two sequences, represented as a simple wrapper around those arrays holding those sequences’ elements for O(1) lookups.
data EditGraph a b = EditGraph { as :: !(Array.Array Int a), bs :: !(Array.Array Int b) }
  deriving (Eq, Show)

-- | Construct an edit graph from Foldable sequences.
makeEditGraph :: (Foldable t, Foldable u) => t a -> u b -> EditGraph a b
makeEditGraph as bs = EditGraph (Array.listArray (0, pred (length as)) (toList as)) (Array.listArray (0, pred (length bs)) (toList bs))

-- | An edit distance, i.e. a cardinal number of changes.
newtype Distance = Distance { unDistance :: Int }
  deriving (Eq, Show)

-- | A diagonal in the edit graph of lists of lengths n and m, numbered from -m to n.
newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Show)

-- | The endpoint of a path through the edit graph, represented as the x/y indices and the script of edits made to get to that point.
data Endpoint a b = Endpoint { x :: !Int, y :: !Int, script :: !(EditScript a b) }
  deriving (Eq, Show)


-- API

-- | Compute the shortest edit script using Myers’ algorithm.
ses :: (HasCallStack, Foldable t, Foldable u) => (a -> b -> Bool) -> t a -> u b -> EditScript a b
ses eq as bs = runMyers eq (makeEditGraph as bs) (M SES `Then` return)


-- Evaluation

-- | Fully evaluate an operation in Myers’ algorithm given a comparator function and an edit graph.
runMyers :: forall a b c. HasCallStack => (a -> b -> Bool) -> EditGraph a b -> Myers a b c -> c
runMyers eq graph step = evalState (go step) (emptyStateForGraph graph)
  where go :: forall c. Myers a b c -> StateT (MyersState a b) Identity c
        go = iterFreerA algebra
        algebra :: forall c x. Step a b x -> (x -> StateT (MyersState a b) Identity c) -> StateT (MyersState a b) Identity c
        algebra step cont = case step of
          M m -> go (decompose eq graph m) >>= cont
          S Get -> get >>= cont
          S (Put s) -> put s >>= cont

-- | Fully evaluate an operation in Myers’ algorithm given a comparator function and an edit graph, returning a list of states and next steps.
runMyersSteps :: HasCallStack => (a -> b -> Bool) -> EditGraph a b -> Myers a b c -> [(MyersState a b, Myers a b c)]
runMyersSteps eq graph = go (emptyStateForGraph graph)
  where go state step = let ?callStack = popCallStack callStack in prefix state step $ case runMyersStep eq graph state step of
          Left result -> [ (state, return result) ]
          Right next -> uncurry go next
        prefix state step = case step of
          Then (M _) _ -> ((state, step) :)
          _ -> identity

-- | Evaluate one step in Myers’ algorithm given a comparator function and an edit graph, returning Either the final result, or the next state and step.
runMyersStep :: HasCallStack => (a -> b -> Bool) -> EditGraph a b -> MyersState a b -> Myers a b c -> Either c (MyersState a b, Myers a b c)
runMyersStep eq graph state step = let ?callStack = popCallStack callStack in case step of
  Return a -> Left a
  Then step cont -> case step of
    M myers -> Right (state, decompose eq graph myers >>= cont)

    S Get -> Right (state, cont state)
    S (Put state') -> Right (state', cont ())


-- | Decompose an operation in Myers’ algorithm into its continuation.
--
--   Dispatches to the per-operation run… functions which implement the meat of the algorithm.
decompose :: HasCallStack => (a -> b -> Bool) -> EditGraph a b -> MyersF a b c -> Myers a b c
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

  SlideFrom from -> runSlideFrom eq graph from
{-# INLINE decompose #-}


-- | Compute the shortest edit script (diff) of an edit graph.
runSES :: HasCallStack => EditGraph a b -> Myers a b (EditScript a b)
runSES (EditGraph as bs)
  | null bs = return (This <$> toList as)
  | null as = return (That <$> toList bs)
  | otherwise = let ?callStack = popCallStack callStack in do
    result <- for [0..(length as + length bs)] (searchUpToD . Distance)
    case result of
      Just (script, _) -> return (reverse script)
      _ -> fail "no shortest edit script found in edit graph (this is a bug in SES.Myers)."

-- | Compute the longest common subsequence of an edit graph.
runLCS :: HasCallStack => EditGraph a b -> Myers a b [(a, b)]
runLCS (EditGraph as bs)
  | null as || null bs = return []
  | otherwise = let ?callStack = popCallStack callStack in do
    result <- M SES `Then` return
    return (catMaybes (these (const Nothing) (const Nothing) ((Just .) . (,)) <$> result))

-- | Compute the edit distance of an edit graph.
runEditDistance :: HasCallStack => EditGraph a b -> Myers a b Int
runEditDistance _ = let ?callStack = popCallStack callStack in length . filter (these (const True) (const True) (const (const False))) <$> (M SES `Then` return)


-- | Search an edit graph for the shortest edit script up to a given proposed edit distance, building on the results of previous searches.
runSearchUpToD :: HasCallStack => EditGraph a b -> Distance -> Myers a b (Maybe (EditScript a b, Distance))
runSearchUpToD (EditGraph as bs) (Distance d) = let ?callStack = popCallStack callStack in
  for [ k | k <- [negate d, negate d + 2 .. d], inRange (negate m, n) k ] (searchAlongK (Distance d) . Diagonal)
  where (n, m) = (length as, length bs)

-- | Search an edit graph for the shortest edit script along a specific diagonal.
runSearchAlongK :: HasCallStack => EditGraph a b -> Distance -> Diagonal -> Myers a b (Maybe (EditScript a b, Distance))
runSearchAlongK (EditGraph as bs) d k = let ?callStack = popCallStack callStack in do
  Endpoint x y script <- moveFromAdjacent d k
  if x >= length as && y >= length bs then
    return (Just (script, d))
  else
    continue

-- | Move onto a given diagonal from one of its in-bounds adjacent diagonals (if any), and slide down any diagonal edges eagerly.
runMoveFromAdjacent :: HasCallStack => EditGraph a b -> Distance -> Diagonal -> Myers a b (Endpoint a b)
runMoveFromAdjacent (EditGraph as bs) (Distance d) (Diagonal k) = let ?callStack = popCallStack callStack in do
  let (n, m) = (length as, length bs)
  from <- if d == 0 || k < negate m || k > n then
    -- The top-left corner, or otherwise out-of-bounds.
    return (Endpoint 0 0 [])
  else if k == negate d || k == negate m then
    -- The lower/left extent of the search region or edit graph, whichever is smaller.
    getK (Diagonal (succ k)) >>= moveDownFrom
  else if k /= d && k /= n then do
    -- Somewhere in the interior of the search region and edit graph.
    prev <- getK (Diagonal (pred k))
    next <- getK (Diagonal (succ k))
    if x prev < x next then
      moveDownFrom next
    else
      moveRightFrom prev
  else
    -- The upper/right extent of the search region or edit graph, whichever is smaller.
    getK (Diagonal (pred k)) >>= moveRightFrom
  endpoint <- slideFrom from
  setK (Diagonal k) endpoint
  return endpoint

-- | Move downward from a given vertex, inserting the element for the corresponding row.
runMoveDownFrom :: HasCallStack => EditGraph a b -> Endpoint a b -> Myers a b (Endpoint a b)
runMoveDownFrom (EditGraph _ bs) (Endpoint x y script) = return (Endpoint x (succ y) (if y < length bs then That (bs ! y) : script else script))

-- | Move rightward from a given vertex, deleting the element for the corresponding column.
runMoveRightFrom :: HasCallStack => EditGraph a b -> Endpoint a b -> Myers a b (Endpoint a b)
runMoveRightFrom (EditGraph as _) (Endpoint x y script) = return (Endpoint (succ x) y (if x < length as then This (as ! x) : script else script))

-- | Return the maximum extent reached and path taken along a given diagonal.
runGetK :: HasCallStack => EditGraph a b -> Diagonal -> Myers a b (Endpoint a b)
runGetK graph k = let ?callStack = popCallStack callStack in do
  v <- checkK graph k
  let (x, script) = v ! unDiagonal k in return (Endpoint x (x - unDiagonal k) script)

-- | Update the maximum extent reached and path taken along a given diagonal.
runSetK :: HasCallStack => EditGraph a b -> Diagonal -> Endpoint a b -> Myers a b ()
runSetK graph k (Endpoint x _ script) = let ?callStack = popCallStack callStack in do
  v <- checkK graph k
  put (MyersState (v Array.// [(unDiagonal k, (x, script))]))

-- | Slide down any diagonal edges from a given vertex.
runSlideFrom :: HasCallStack => (a -> b -> Bool) -> EditGraph a b -> Endpoint a b -> Myers a b (Endpoint a b)
runSlideFrom eq (EditGraph as bs) (Endpoint x y script)
  | x >= 0, x < length as
  , y >= 0, y < length bs
  , a <- as ! x
  , b <- bs ! y
  , a `eq` b  = slideFrom  (Endpoint (succ x) (succ y) (These a b : script))
  | otherwise = return (Endpoint       x        y               script)


-- Smart constructors

-- | Compute the longest common subsequence.
lcs :: HasCallStack => Myers a b [(a, b)]
lcs = M LCS `Then` return

-- | Compute the edit distance.
editDistance :: HasCallStack => Myers a b Int
editDistance = M EditDistance `Then` return

-- | Search an edit graph for the shortest edit script up to a given proposed edit distance, building on the results of previous searches.
searchUpToD :: HasCallStack => Distance -> Myers a b (Maybe (EditScript a b, Distance))
searchUpToD distance = M (SearchUpToD distance) `Then` return

-- | Search an edit graph for the shortest edit script along a specific diagonal.
searchAlongK :: HasCallStack => Distance -> Diagonal -> Myers a b (Maybe (EditScript a b, Distance))
searchAlongK d k = M (SearchAlongK d k) `Then` return

-- | Move onto a given diagonal from one of its in-bounds adjacent diagonals (if any), and slide down any diagonal edges eagerly.
moveFromAdjacent :: HasCallStack => Distance -> Diagonal -> Myers a b (Endpoint a b)
moveFromAdjacent d k = M (MoveFromAdjacent d k) `Then` return

-- | Move downward from a given vertex, inserting the element for the corresponding row.
moveDownFrom :: HasCallStack => Endpoint a b -> Myers a b (Endpoint a b)
moveDownFrom e = M (MoveDownFrom e) `Then` return

-- | Move rightward from a given vertex, deleting the element for the corresponding column.
moveRightFrom :: HasCallStack => Endpoint a b -> Myers a b (Endpoint a b)
moveRightFrom e = M (MoveRightFrom e) `Then` return

-- | Return the maximum extent reached and path taken along a given diagonal.
getK :: HasCallStack => Diagonal -> Myers a b (Endpoint a b)
getK diagonal = M (GetK diagonal) `Then` return

-- | Update the maximum extent reached and path taken along a given diagonal.
setK :: HasCallStack => Diagonal -> Endpoint a b -> Myers a b ()
setK diagonal x = M (SetK diagonal x) `Then` return

-- | Slide down any diagonal edges from a given vertex.
slideFrom :: HasCallStack => Endpoint a b -> Myers a b (Endpoint a b)
slideFrom from = M (SlideFrom from) `Then` return


-- Implementation details

-- | The state stored by Myers’ algorithm; an array of m + n + 1 values indicating the maximum x-index reached and path taken along each diagonal.
newtype MyersState a b = MyersState { unMyersState :: Array.Array Int (Int, EditScript a b) }
  deriving (Eq, Show)

-- | State effect used in Myers.
data State s a where
  Get :: State s s
  Put :: s -> State s ()

-- | Compute the empty state of length m + n + 1 for a given edit graph.
emptyStateForGraph :: EditGraph a b -> MyersState a b
emptyStateForGraph (EditGraph as bs) = let (n, m) = (length as, length bs) in
  MyersState (Array.listArray (negate m, n) (repeat (0, [])))

-- | Evaluate some function for each value in a list until one returns a value or the list is exhausted.
for :: [a] -> (a -> Myers c d (Maybe b)) -> Myers c d (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

-- | Continue evaluation of a for loop without returning a value. To exit the loop without continuing, return a value in 'Just' instead.
continue :: Myers b c (Maybe a)
continue = return Nothing


-- | Throw a failure. Used to indicate an error in the implementation of Myers’ algorithm.
fail :: (HasCallStack, Monad m) => String -> m a
fail s = let ?callStack = fromCallSiteList (filter ((/= "M") . fst) (getCallStack callStack)) in
  throw (MyersException s callStack)

-- | Bounds-checked indexing of arrays, preserving the call stack.
(!) :: HasCallStack => Array.Array Int a -> Int -> a
v ! i | inRange (Array.bounds v) i = v Array.! i
      | otherwise = let ?callStack = fromCallSiteList (filter ((/= "M") . fst) (getCallStack callStack)) in
          throw (MyersException ("index " <> show i <> " out of bounds") callStack)

-- | Check that a given diagonal is in-bounds for the edit graph, returning the actual index to use and the state array.
checkK :: HasCallStack => EditGraph a b -> Diagonal -> Myers a b (Array.Array Int (Int, EditScript a b))
checkK _ (Diagonal k) = let ?callStack = popCallStack callStack in do
  v <- gets unMyersState
  unless (inRange (Array.bounds v) k) $ fail ("diagonal " <> show k <> " outside state bounds " <> show (Array.bounds v))
  return v


-- | Lifted showing of arrays.
liftShowsVector :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array.Array Int a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList

-- | Lifted showing of operations in Myers’ algorithm.
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
  SlideFrom endpoint -> showsUnaryWith (liftShowsEndpoint sp1 sp2) "SlideFrom" d endpoint

-- | Lifted showing of ternary constructors.
showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $
  showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

-- | Lifted showing of State.
liftShowsState :: (Int -> a -> ShowS) -> Int -> State a b -> ShowS
liftShowsState sp d state = case state of
  Get -> showString "Get"
  Put s -> showsUnaryWith sp "Put" d s

-- | Lift value/list showing functions into a showing function for steps in Myers’ algorithm.
liftShowsStep :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> Step a b c -> ShowS
liftShowsStep sp1 sl1 sp2 sl2 d step = case step of
  M m -> showsUnaryWith (liftShowsMyersF sp1 sp2) "M" d m
  S s -> showsUnaryWith (liftShowsState (liftShowsPrec2 sp1 sl1 sp2 sl2)) "S" d s

-- | Lifted showing of These.
liftShowsThese :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> These a b -> ShowS
liftShowsThese sa sb d t = case t of
  This a -> showsUnaryWith sa "This" d a
  That b -> showsUnaryWith sb "That" d b
  These a b -> showsBinaryWith sa sb "These" d a b

-- | Lifted showing of edit scripts.
liftShowsEditScript :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> EditScript a b -> ShowS
liftShowsEditScript sa sb _ = showListWith (liftShowsThese sa sb 0)

-- | Lifted showing of edit graph endpoints.
liftShowsEndpoint :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> Endpoint a b -> ShowS
liftShowsEndpoint sp1 sp2 d (Endpoint x y script) = showsTernaryWith showsPrec showsPrec (liftShowsEditScript sp1 sp2) "Endpoint" d x y script

-- | Exceptions in Myers’ algorithm, along with a description and call stack.
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

instance (Show a, Show b) => Show1 (Step a b) where
  liftShowsPrec _ _ = liftShowsStep showsPrec showList showsPrec showList

instance (Show a, Show b) => Show (Step a b c) where
  showsPrec = liftShowsStep showsPrec showList showsPrec showList

instance Exception MyersException

instance Show MyersException where
  showsPrec _ (MyersException s c) = showString "Exception: " . showString s . showChar '\n' . showString (prettyCallStack c)
