{-# LANGUAGE Strict #-}
module SES where

import Data.Array.MArray
import Data.Array.ST
import Data.These
import Prologue


-- | Edit constructor for two terms, if comparable. Otherwise returns Nothing.
type Comparable term = term -> term -> Bool

-- | A function that computes the cost of an edit.
type Cost term = These term term -> Int

-- | Find the shortest edit script (diff) between two terms given a function to compute the cost.
ses :: Comparable term -> Cost term -> [term] -> [term] -> [These term term]
ses canCompare cost as bs = runST $ do
  array <- newArray (0, indexKey (length as) (length bs) (length bs)) Nothing
  editScript <- diffAt array (length bs) canCompare cost (0, 0) as bs
  pure $ fst <$> editScript

-- | Find the shortest edit script between two terms at a given vertex in the edit graph.
diffAt :: STArray s Int (Maybe [(These term term, Int)]) -> Int -> Comparable term -> Cost term -> (Int, Int) -> [term] -> [term] -> ST s [(These term term, Int)]
diffAt array lenJ canCompare cost (i, j) as bs
  | (a : as) <- as, (b : bs) <- bs = do
    el <- readArray array index
    case el of
      Just diffs -> pure diffs
      Nothing -> do
        down <- recur (i, succ j) as (b : bs)
        right <- recur (succ i, j) (a : as) bs
        nomination <- best <$> if canCompare a b
          then do
            diagonal <- recur (succ i, succ j) as bs
            pure [ delete a down, insert b right, consWithCost cost (These a b) diagonal ]
          else pure [ delete a down, insert b right ]
        writeArray array index (Just nomination)
        pure nomination
  | null as = pure $ foldr insert [] bs
  | null bs = pure $ foldr delete [] as
  | otherwise = pure []
  where
    index = indexKey i j lenJ
    delete = consWithCost cost . This
    insert = consWithCost cost . That
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    recur = diffAt array lenJ canCompare cost

indexKey :: Int -> Int -> Int -> Int
indexKey i j lenJ = j + i * lenJ

-- | Prepend an edit script and the cumulative cost onto the edit script.
consWithCost :: Cost term -> These term term -> [(These term term, Int)] -> [(These term term, Int)]
consWithCost cost edit rest = (edit, cost edit + maybe 0 snd (fst <$> uncons rest)) : rest
