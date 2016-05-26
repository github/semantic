{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Adjoined where

import Prologue hiding (uncons, (:<))
import Data.Sequence as Seq hiding (null)
import Data.Align
import Data.Bifunctor.These
import Data.Coalescent

-- | A collection of elements which can be adjoined onto other such collections associatively. There are two big wins with Data.Adjoined:
-- |
-- | 1. Efficient adjoining of lines and concatenation, thanks to its use of Data.Sequence’s `Seq` type.
-- | 2. The Monoid instance guarantees that adjoining cannot touch any lines other than the outermost.
-- |
-- | Since aligning diffs proceeds through the diff tree depth-first, adjoining child nodes and context from right to left, the former is crucial for efficiency, and the latter is crucial for correctness. Prior to using Data.Adjoined, repeatedly adjoining the last line in a node into its parent, and then its grandparent, and so forth, would sometimes cause blank lines to “travel” downwards, ultimately shifting blank lines at the end of nodes down proportionately to the depth in the tree at which they were introduced.
newtype Adjoined a = Adjoined { unAdjoined :: Seq a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Construct an Adjoined from a list.
fromList :: [a] -> Adjoined a
fromList = Adjoined . Seq.fromList

-- | Construct Adjoined by adding an element at the left.
cons :: a -> Adjoined a -> Adjoined a
cons a (Adjoined as) = Adjoined (a <| as)

-- | Destructure a non-empty Adjoined into Just the leftmost element and the rightward remainder of the Adjoined, or Nothing otherwise.
uncons :: Adjoined a -> Maybe (a, Adjoined a)
uncons (Adjoined v) | a :< as <- viewl v = Just (a, Adjoined as)
                    | otherwise = Nothing

-- | Construct Adjoined by adding an element at the right.
snoc :: Adjoined a -> a -> Adjoined a
snoc (Adjoined as) a = Adjoined (as |> a)

-- | Destructure a non-empty Adjoined into Just the rightmost element and the leftward remainder of the Adjoined, or Nothing otherwise.
unsnoc :: Adjoined a -> Maybe (Adjoined a, a)
unsnoc (Adjoined v) | as :> a <- viewr v = Just (Adjoined as, a)
                    | otherwise = Nothing

instance Applicative Adjoined where
  pure = return
  (<*>) = ap

instance Alternative Adjoined where
  empty = Adjoined Seq.empty
  Adjoined a <|> Adjoined b = Adjoined (a >< b)

instance Monad Adjoined where
  return = Adjoined . pure
  a >>= f | Just (a, as) <- uncons a = f a <|> (as >>= f)
          | otherwise = Adjoined Seq.empty

instance Coalescent a => Monoid (Adjoined a) where
  mempty = Adjoined Seq.empty
  a `mappend` b | Just (as, a) <- unsnoc a,
                  Just (b, bs) <- uncons b
                = as <|> coalesce a b <|> bs
                | otherwise = Adjoined (unAdjoined a >< unAdjoined b)

instance Align Adjoined where
  nil = Adjoined Seq.empty
  align as bs | Just (as, a) <- unsnoc as,
                Just (bs, b) <- unsnoc bs = align as bs `snoc` These a b
              | null bs = This <$> as
              | null as = That <$> bs
              | otherwise = nil
