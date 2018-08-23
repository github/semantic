{-# LANGUAGE GADTs, TypeOperators #-}

module Control.Abstract.Matching
  ( Matcher
  , TermMatcher
  , target
  , ensure
  , match
  , matchM
  , narrow
  , narrow'
  , succeeds
  , fails
  , runMatcher
  , stepMatcher
  ) where

import           Data.Algebra
import           Data.Sum
import           Data.Term
import           Prologue hiding (project)

-- | A @Matcher t a@ is a tree automaton that matches some 'Recursive' and 'Corecursive' type @t@, yielding values of type @a@.
--   Matching operations are implicitly recursive: when you run a 'Matcher', it is applied bottom-up.
--   If a matching operation returns a value, it is assumed to have succeeded. You use the 'guard', 'narrow', and 'ensure'
--   functions to control whether a given datum is matched. The @t@ datum matched by a matcher is immutable; future APIs will
--   provide the ability to rewrite and change these data.
data Matcher t a where
  -- TODO: Choice is inflexible and slow. A Sum over fs can be queried for its index, and we can build a jump table over that.
  -- We can copy NonDet to have fair conjunction or disjunction.
  Choice :: Matcher t a -> Matcher t a -> Matcher t a
  Target :: Matcher t t
  Empty  :: Matcher t a
  -- We could have implemented this by changing the semantics of how Then is interpreted, but that would make Then and Sequence inconsistent.
  Match  :: (t -> Maybe u) -> Matcher u a -> Matcher t a
  Pure   :: a -> Matcher t a
  Then   :: Matcher t b -> (b -> Matcher t a) -> Matcher t a

-- | A convenience alias for matchers that both target and return 'Term' values.
type TermMatcher fs ann = Matcher (Term (Sum fs) ann) (Term (Sum fs) ann)

instance Functor (Matcher t) where
  fmap = liftA

instance Applicative (Matcher t) where
  pure   = Pure
  -- We can add a Sequence constructor to optimize this when we need.
  (<*>)  = ap

instance Alternative (Matcher t) where
  empty = Empty
  (<|>) = Choice

instance Monad (Matcher t) where
  (>>=) = Then

-- | This matcher always succeeds.
succeeds :: Matcher t ()
succeeds = guard True

-- | This matcher always fails.
fails :: Matcher t ()
fails = guard False

-- | 'target' extracts the 't' that a given 'Matcher' is operating upon.
--   Similar to a reader monad's 'ask' function.
target :: Matcher t t
target = Target

-- | 'ensure' succeeds iff the provided predicate function returns true when applied to the matcher's 'target'.
ensure :: (t -> Bool) -> Matcher t ()
ensure f = target >>= \c -> guard (f c)

-- | 'matchm' takes a modification function and a new matcher action the target parameter of which
--   is the result of the modification function. If the modification function returns 'Just' when
--   applied to the current 'target', the given matcher is executed with the result of that 'Just'
--   as the new target; if 'Nothing' is returned, the action fails.
matchM :: (t -> Maybe u) -> Matcher u a -> Matcher t a
matchM = Match

-- | 'match' is a more specific version of 'matchM' optimized for targeting union types. If the target
--   can be projected to the type expected by the modification function, the provided matcher action will
--   execute. An example:
--
-- @
-- integerMatcher :: (Lit.Integer :< fs) => Matcher (Term (Sum fs) ann) ByteString
-- integerMatcher = match Lit.integerContent target
-- @
--
-- @integerMatcher@ accepts any union type that contains an integer literal, and only succeeds if the
-- target in question is actually an integer literal.
match :: (f :< fs)
      => (f (Term (Sum fs) ann) -> b)
      -> Matcher b a
      -> Matcher (Term (Sum fs) ann) a
match f = Match (fmap f . projectTerm)

-- | @narrow'@ attempts to project a union-type target to a more specific type.
narrow' :: (f :< fs) => Matcher (Term (Sum fs) ann) (Maybe (f (Term (Sum fs) ann)))
narrow' = fmap projectTerm Target

-- | 'narrow' behaves as @narrow'@, but fails if the target cannot be thus projected.
narrow :: (f :< fs) => Matcher (Term (Sum fs) ann) (f (Term (Sum fs) ann))
narrow = narrow' >>= foldMapA pure

-- | The entry point for executing matchers.
--   The Alternative parameter should be specialized by the calling context. If you want a single
--   result, specialize it to 'Maybe'; if you want a list of all terms and subterms matched by the
--   provided 'Matcher' action, specialize it to '[]'.
runMatcher :: (Alternative m, Monad m, Corecursive t, Recursive t, Foldable (Base t))
           => Matcher t a
           -> t
           -> m a
runMatcher m = para (paraMatcher m)

paraMatcher :: (Alternative m, Monad m, Corecursive t, Foldable (Base t)) => Matcher t a -> RAlgebra (Base t) t (m a)
paraMatcher m t = stepMatcher (embedTerm t) m <|> foldMapA snd t

-- | Run one step of a 'Matcher' computation. Look at 'runMatcher' if you want something
-- that folds over subterms.
stepMatcher :: (Alternative m, Monad m) => t -> Matcher t a -> m a
stepMatcher t (Choice a b) = stepMatcher t a <|> stepMatcher t b
stepMatcher t Target       = pure t
stepMatcher t (Match f m)  = foldMapA (`stepMatcher` m) (f t)
stepMatcher _ (Pure a)     = pure a
stepMatcher _ Empty        = empty
stepMatcher t (Then m f)   = stepMatcher t m >>= stepMatcher t . f
