{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
module SES.Myers where

import Control.Exception
import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.String
import Data.These
import qualified Data.Vector as Vector
import GHC.Show hiding (show)
import GHC.Stack
import Prologue hiding (for, State, error)
import Text.Show (showListWith)

data MyersF a b result where
  SES :: EditGraph a b -> MyersF a b (EditScript a b)
  LCS :: EditGraph a b -> MyersF a b [(a, b)]
  EditDistance :: EditGraph a b -> MyersF a b Int
  SearchUpToD :: EditGraph a b -> Distance -> MyersF a b (Maybe (EditScript a b, Distance))
  SearchAlongK :: EditGraph a b -> Distance -> Diagonal -> MyersF a b (Maybe (EditScript a b, Distance))
  FindDPath :: EditGraph a b -> Distance -> Diagonal -> MyersF a b Endpoint

  GetK :: EditGraph a b -> Diagonal -> MyersF a b (Endpoint, EditScript a b)
  SetK :: EditGraph a b -> Diagonal -> Int -> EditScript a b -> MyersF a b ()

  Slide :: EditGraph a b -> Endpoint -> EditScript a b -> MyersF a b (Endpoint, EditScript a b)

type EditScript a b = [These a b]

data State s a where
  Get :: State s s
  Put :: s -> State s ()

data StepF a b result where
  M :: HasCallStack => MyersF a b c -> StepF a b c
  S :: State (MyersState a b) c -> StepF a b c
  GetEq :: StepF a b (a -> b -> Bool)

type Myers a b = Freer (StepF a b)

data EditGraph a b = EditGraph { as :: !(Vector.Vector a), bs :: !(Vector.Vector b) }
  deriving (Eq, Show)

makeEditGraph :: (Foldable t, Foldable u) => t a -> u b -> EditGraph a b
makeEditGraph as bs = EditGraph (Vector.fromList (toList as)) (Vector.fromList (toList bs))

newtype Distance = Distance { unDistance :: Int }
  deriving (Eq, Show)

newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Show)

data Endpoint = Endpoint { x :: !Int, y :: !Int }
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
      result <- for [0..maxD] (searchUpToD graph . Distance)
      case result of
        Just (script, _) -> return script
        _ -> fail "no shortest edit script found in edit graph (this is a bug in SES.Myers)."

  EditDistance graph -> length . filter (these (const True) (const True) (const (const False))) <$> ses graph

  SearchUpToD graph (Distance d) -> for [negate d, negate d + 2 .. d] (searchAlongK graph (Distance d) . Diagonal)

  SearchAlongK graph d k -> do
    Endpoint x y <- findDPath graph d k
    if x >= n && y >= m then do
      (_, script) <- getK graph k
      return (Just (script, d))
    else
      continue

  FindDPath graph (Distance d) (Diagonal k) -> do
    (from, fromScript) <- if d == 0 then
      return (Endpoint 0 0, [])
    else if k == negate d then do
      (Endpoint nextX nextY, nextScript) <- getK graph (Diagonal (succ k))
      return (Endpoint nextX (succ nextY),     addInBounds bs nextY That nextScript) -- downward (insertion)
    else if k /= d then do
      (Endpoint prevX prevY, prevScript) <- getK graph (Diagonal (pred k))
      (Endpoint nextX nextY, nextScript) <- getK graph (Diagonal (succ k))
      return $ if prevX < nextX then
        (Endpoint nextX (succ nextY), addInBounds bs nextY That nextScript) -- downward (insertion)
      else
        (Endpoint (succ prevX) prevY, addInBounds as prevX This prevScript) -- rightward (deletion)
    else do
      (Endpoint prevX prevY, prevScript) <- getK graph (Diagonal (pred k))
      return (Endpoint (succ prevX) prevY, addInBounds as prevX This prevScript) -- rightward (deletion)
    (endpoint, script) <- slide graph from fromScript
    setK graph (Diagonal k) (x endpoint) script
    return endpoint
    where addInBounds :: Vector.Vector a -> Int -> (a -> b) -> [b] -> [b]
          addInBounds v i with to = if i >= 0 && i < length v then with (v Vector.! i) : to else to

  GetK _ (Diagonal k) -> do
    v <- gets unMyersState
    let i = index v k
    when (i < 0) $
      fail ("diagonal " <> show k <> " (" <> show i <> ") underflows state indices " <> show (negate maxD) <> ".." <> show maxD <> " (0.." <> show (2 * maxD) <> ")")
    when (i >= length v) $
      fail ("diagonal " <> show k <> " (" <> show i <> ") overflows state indices " <> show (negate maxD) <> ".." <> show maxD <> " (0.." <> show (2 * maxD) <> ")")
    let (x, script) = v Vector.! i in return (Endpoint x (x - k), script)

  SetK _ (Diagonal k) x script ->
    modify (MyersState . set . unMyersState)
    where set v = v Vector.// [(index v k, (x, script))]

  Slide graph (Endpoint x y) script
    | x >= 0, x < n
    , y >= 0, y < m -> do
      eq <- getEq
      let a = as Vector.! x
      let b = bs Vector.! y
      if a `eq` b then
        slide graph (Endpoint (succ x) (succ y)) (These a b : script)
      else
        return (Endpoint x y, script)
    | otherwise -> return (Endpoint x y, script)

  where (EditGraph as bs, n, m, maxD) = editGraph myers

        fail :: (HasCallStack, Monad m) => String -> m a
        fail s = let ?callStack = fromCallSiteList (filter ((/= "M") . fst) (getCallStack callStack)) in
          throw (MyersException s callStack)


-- Smart constructors

ses :: HasCallStack => EditGraph a b -> Myers a b (EditScript a b)
ses graph = M (SES graph) `Then` return

lcs :: HasCallStack => EditGraph a b -> Myers a b [(a, b)]
lcs graph = M (LCS graph) `Then` return

editDistance :: HasCallStack => EditGraph a b -> Myers a b Int
editDistance graph = M (EditDistance graph) `Then` return

searchUpToD :: HasCallStack => EditGraph a b -> Distance -> Myers a b (Maybe (EditScript a b, Distance))
searchUpToD graph distance = M (SearchUpToD graph distance) `Then` return

searchAlongK :: HasCallStack => EditGraph a b -> Distance -> Diagonal -> Myers a b (Maybe (EditScript a b, Distance))
searchAlongK graph d k = M (SearchAlongK graph d k) `Then` return

findDPath :: HasCallStack => EditGraph a b -> Distance -> Diagonal -> Myers a b Endpoint
findDPath graph d k = M (FindDPath graph d k) `Then` return

getK :: HasCallStack => EditGraph a b -> Diagonal -> Myers a b (Endpoint, EditScript a b)
getK graph diagonal = M (GetK graph diagonal) `Then` return

setK :: HasCallStack => EditGraph a b -> Diagonal -> Int -> EditScript a b -> Myers a b ()
setK graph diagonal x script = M (SetK graph diagonal x script) `Then` return

slide :: HasCallStack => EditGraph a b -> Endpoint -> EditScript a b -> Myers a b (Endpoint, EditScript a b)
slide graph from script = M (Slide graph from script) `Then` return

getEq :: HasCallStack => Myers a b (a -> b -> Bool)
getEq = GetEq `Then` return


-- Implementation details

newtype MyersState a b = MyersState { unMyersState :: Vector.Vector (Int, EditScript a b) }
  deriving (Eq, Show)

emptyStateForStep :: Myers a b c -> MyersState a b
emptyStateForStep step = case step of
  Then (M myers) _ ->
    let (_, n, m, _) = editGraph myers in
    MyersState (Vector.replicate (succ (m + n)) (0, []))
  _ -> MyersState Vector.empty

for :: [a] -> (a -> Myers c d (Maybe b)) -> Myers c d (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all

continue :: Myers b c (Maybe a)
continue = return Nothing

ceilDiv :: Integral a => a -> a -> a
ceilDiv = (uncurry (+) .) . divMod

index :: Vector.Vector a -> Int -> Int
index v k = if k >= 0 then k else length v + k

divideGraph :: EditGraph a b -> Endpoint -> (EditGraph a b, EditGraph a b)
divideGraph (EditGraph as bs) (Endpoint x y) =
  ( EditGraph (slice 0  x              as) (slice 0  y              bs)
  , EditGraph (slice x (length as - x) as) (slice y (length bs - y) bs) )
  where slice from to v = Vector.slice (max 0 (min from (length v))) (max 0 (min to (length v))) v


editGraph :: MyersF a b c -> (EditGraph a b, Int, Int, Int)
editGraph myers = (EditGraph as bs, n, m, (m + n) `ceilDiv` 2)
  where EditGraph as bs = case myers of
          SES g -> g
          LCS g -> g
          EditDistance g -> g
          SearchUpToD g _ -> g
          SearchAlongK g _ _ -> g
          FindDPath g _ _ -> g
          GetK g _ -> g
          SetK g _ _ _ -> g
          Slide g _ _ -> g
        (n, m) = (length as, length bs)


liftShowsVector :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Vector.Vector a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList

liftShowsMyersF :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> MyersF a b c -> ShowS
liftShowsMyersF sp1 sl1 sp2 sl2 d m = case m of
  SES graph -> showsUnaryWith showGraph "SES" d graph
  LCS graph -> showsUnaryWith showGraph "LCS" d graph
  EditDistance graph -> showsUnaryWith showGraph "EditDistance" d graph
  SearchUpToD graph distance -> showsBinaryWith showGraph showsPrec "SearchUpToD" d graph distance
  SearchAlongK graph distance diagonal -> showsTernaryWith showGraph showsPrec showsPrec "SearchAlongK" d graph distance diagonal
  FindDPath graph distance diagonal -> showsTernaryWith showGraph showsPrec showsPrec "FindDPath" d graph distance diagonal
  GetK graph diagonal -> showsBinaryWith showGraph showsPrec "GetK" d graph diagonal
  SetK graph diagonal v script -> showsQuaternaryWith showGraph showsPrec showsPrec (liftShowsEditScript sp1 sp2) "SetK" d graph diagonal v script
  Slide graph endpoint script -> showsTernaryWith showGraph showsPrec (liftShowsEditScript sp1 sp2) "Slide" d graph endpoint script
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

instance (Show a, Show b) => Show1 (MyersF a b) where
  liftShowsPrec _ _ = liftShowsMyersF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show (MyersF a b c) where
  showsPrec = liftShowsMyersF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show1 (StepF a b) where
  liftShowsPrec _ _ = liftShowsStepF showsPrec showList showsPrec showList

instance (Show a, Show b) => Show (StepF a b c) where
  showsPrec = liftShowsStepF showsPrec showList showsPrec showList

instance Exception MyersException

instance Show MyersException where
  showsPrec _ (MyersException s c) = showString "Exception: " . showString s . showChar '\n' . showString (prettyCallStack c)
