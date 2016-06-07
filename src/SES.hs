module SES where

import Control.Parallel.Strategies
import Data.List ((!!), genericLength)
import qualified Data.Map as Map
import Patch
import Prologue
import Unsafe
import Data.String


-- | Edit constructor for two terms, if comparable. Otherwise returns Nothing.
type Compare term edit = term -> term -> Maybe edit

-- | A function that computes the cost of an edit.
type Cost edit = edit -> Integer

-- | Find the shortest edit script (diff) between two terms given a function to compute the cost.
ses :: Applicative edit => Compare term (edit (Patch term)) -> Cost (edit (Patch term)) -> [term] -> [term] -> [edit (Patch term)]
ses = ses'
-- ses diffTerms cost as bs = fst <$> evalState diffState Map.empty where
--   diffState = diffAt diffTerms cost (0, 0) as bs

-- | Find the shortest edit script between two terms at a given vertex in the edit graph.
diffAt :: Applicative edit => Compare term (edit (Patch term)) -> Cost (edit (Patch term)) -> (Integer, Integer) -> [term] -> [term] -> State (Map.Map (Integer, Integer) [(edit (Patch term), Integer)]) [(edit (Patch term), Integer)]
diffAt diffTerms cost (i, j) as bs
  | (a : as) <- as, (b : bs) <- bs = do
  cachedDiffs <- get
  case Map.lookup (i, j) cachedDiffs of
    Just diffs -> pure diffs
    Nothing -> do
      down <- recur (i, succ j) as (b : bs)
      right <- recur (succ i, j) (a : as) bs
      nomination <- best <$> case diffTerms a b of
        Just diff -> do
          diagonal <- recur (succ i, succ j) as bs
          pure [ delete a down, insert b right, consWithCost cost diff diagonal ]
        Nothing -> pure [ delete a down, insert b right ]
      cachedDiffs' <- get
      put $ Map.insert (i, j) nomination cachedDiffs'
      pure nomination
  | null as = pure $ foldr insert [] bs
  | null bs = pure $ foldr delete [] as
  | otherwise = pure []
  where
    delete = consWithCost cost . pure . Delete
    insert = consWithCost cost . pure . Insert
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    recur = diffAt diffTerms cost


ses' :: Applicative edit => Compare term (edit (Patch term)) -> Cost (edit (Patch term)) -> [term] -> [term] -> [edit (Patch term)]
ses' diffTerms cost as bs = trace "ses'" $ fst <$> diffAtMemo 0 0
  where diffAtMemo = fix (memoize . memoize . curry . diffAt' diffTerms cost (indexMemo as) (indexMemo bs) . uncurry)
        -- diffByColumn = fix (memoize . diffAt' diffTerms cost (indexMemo as) (indexMemo bs))
        index :: [a] -> (Int -> Maybe a) -> Int -> Maybe a
        index elements recur i =
          if i < length elements then Just (elements !! i) else Nothing
        indexMemo list = fix (memoize . index list)

diffAt' :: Applicative edit => Compare term (edit (Patch term)) -> Cost (edit (Patch term)) -> (Int -> Maybe term) -> (Int -> Maybe term) -> ((Int, Int) -> [(edit (Patch term), Integer)]) -> (Int, Int) -> [(edit (Patch term), Integer)]
diffAt' diffTerms cost as bs recur (i, j)
  | Just a <- as i, Just b <- bs j = trace ("inner: (" ++ show i ++ ", " ++ show j ++ ")") $ do
    let down = recur (i, succ j)
        right = recur (succ i, j) in
        best $ case diffTerms a b of
          Just diff ->
            let diagonal = recur (succ i, succ j) in
                [ delete a down, insert b right, consWithCost cost diff diagonal ]
          Nothing -> [ delete a down, insert b right ]
  | Nothing <- as i, Just b <- bs j = insert b (recur (i, succ j))
  | Nothing <- bs j, Just a <- as i = delete a (recur (succ i, j))
  | otherwise = []
  where
    delete = consWithCost cost . pure . Delete
    insert = consWithCost cost . pure . Insert
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    -- recur = diffAt' diffTerms cost

memoize :: (Int -> a) -> (Int -> a)
memoize f = (fmap f [0 ..] !!)

memoizeEnum :: Enum a => (a -> b) -> a -> b
memoizeEnum f = (fmap f [toEnum 0 ..] !!) . fromEnum

memoize2d :: Int -> (Int -> Int -> a) -> (Int -> Int -> a)
memoize2d width f = outof (memoize (into f))
  where into f i = f (i `div` width) (i `mod` width)
        outof f i j = f (i * width + j)

memoize2 :: ((Int, Int) -> a) -> ((Int, Int) -> a)
memoize2 f = fromJust . (`lookup` memo)
  where memo = zipWith (\ i j -> ((i, j), f (i, j))) [0 ..] [0 ..]

-- | Prepend an edit script and the cumulative cost onto the edit script.
consWithCost :: Cost edit -> edit -> [(edit, Integer)] -> [(edit, Integer)]
consWithCost cost edit rest = (edit, (cost edit `using` rpar) + maybe 0 snd (fst <$> uncons rest)) : rest


benchmarkSES :: [String] -> [String] -> [Either String (Patch String)]
benchmarkSES as bs = ses compare cost as bs
  where compare a b = if a == b then Just (Left a) else Nothing
        cost = either (const 0) (sum . fmap genericLength)
