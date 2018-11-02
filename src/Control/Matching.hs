{-# LANGUAGE GADTs, TypeOperators #-}

module Control.Matching
  ( -- | Core types
    Matcher
  , TermMatcher
  -- | Combinators
  , target
  , purely
  , ensure
  , succeeds
  , fails
  -- | Predicate filtering
  , match
  , only
  -- | Projecting terms and sums
  , narrow
  , narrowF
  , need
  , (>>:)
  -- | Useful matchers
  , mhead
  , mjust
  -- | Running matchers
  , matchRecursively
  , matchOne
  -- | Reexports from Control.Category
  , (>>>)
  , (<<<)
  ) where

import Prelude hiding (id, (.))
import Prologue hiding (project)

import Control.Category

import Data.Algebra
import Data.Sum
import Data.Term

-- | A @Matcher t a@ is a tree automaton that matches some 'Recursive'
--   and 'Corecursive' type @t@, yielding values of type @a@.
--
-- Matching operations are implicitly recursive: when you run a
-- 'Matcher', it is applied bottom-up. If a matching operation
-- returns a value, it is assumed to have succeeded. You use the
-- 'guard', 'narrow', and 'ensure' functions to control whether a
-- given datum is matched. The @t@ datum matched by a matcher is
-- immutable; if you need to modify values during a match operation,
-- consider using Control.Rewriting.
data Matcher t a where
  -- TODO: Choice is inflexible and slow. A Sum over fs can be queried for its index, and we can build a jump table over that.
  -- We can copy NonDet to have fair conjunction or disjunction.
  Choice :: Matcher t a -> Matcher t a -> Matcher t a
  Target :: Matcher t t
  Empty  :: Matcher t a
  Comp   :: Matcher b c -> Matcher a b -> Matcher a c
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

-- | Matchers are generally composed left-to-right with '>>>'.
instance Category Matcher where
  id  = Target
  (.) = Comp

-- | This matcher always succeeds.
succeeds :: Matcher t ()
succeeds = guard True

-- | This matcher always fails.
fails :: Matcher t ()
fails = guard False

-- | 'target' extracts the 't' that a given 'Matcher' is operating upon.
--   Similar to a reader monad's 'ask' function. This is an alias for 'id'
target :: Matcher t t
target = id

-- | 'ensure' succeeds iff the provided predicate function returns true when applied to the matcher's 'target'.
ensure :: (t -> Bool) -> Matcher t ()
ensure f = target >>= \c -> guard (f c)

-- | Promote a pure function to a 'Matcher'.
purely :: (a -> b) -> Matcher a b
purely f = fmap f target

-- | 'match' takes a modification function and a new matcher action
-- the target parameter of which is the result of the modification
-- function. If the modification function returns 'Just' when applied
-- to the current 'target', the given matcher is executed with the
-- result of that 'Just' as the new target; if 'Nothing' is returned,
-- the action fails.
--
-- This is the lowest-level combinator for applying a predicate function
-- to a matcher. In practice, you'll generally use the 'need' and '>>:'
-- combinators to iterate on recursive 'Term' values.
match :: (t -> Maybe u) -> Matcher u a -> Matcher t a
match = Match

-- | An alias for the common pattern of @match f id@.
only :: (t -> Maybe u) -> Matcher t u
only f = Match f Target

-- | The 'need' combinator is the primary interface for creating
-- matchers that 'project' their internal 'Term' values into some
-- constituent type. Given a function from a constituent type @f@
-- @need p@ succeeds if the provided term can be projected into
-- an @f@, then applies the @p@ function.
need :: ( f :< fs
        , term ~ Term (Sum fs) ann
        )
     => (f term -> b)
     -> Matcher term b
need f = Match (fmap f . projectTerm) target

-- | An alias for @need f >>>@. Allows you to avoid repeated
-- calls to 'need' in long chains of projection and composition.
-- Similar to '>>^' from Control.Arrow, except the call to 'projectTerm'
-- is implicit.
infixr 0 >>:
(>>:) :: (f :< fs, term ~ Term (Sum fs) ann)
      => (f term -> b)
      -> Matcher b c
      -> Matcher term c
f >>: a = need f >>> a

-- | 'narrow' projects the given 'Term' of 'Sum's into a constituent member
-- of that 'Sum', failing if the target cannot be thus projected.
narrow :: (f :< fs) => Matcher (Term (Sum fs) ann) (f (Term (Sum fs) ann))
narrow = purely projectTerm >>= foldMapA pure

-- | Like 'narrow', but it returns the result of the projection
-- in a 'TermF'. Useful for returning a matched node after ensuring
-- its contents are valid, e.g @narrowF <* a >:: b >>> ensure f@
narrowF :: (f :< fs, term ~ Term (Sum fs) ann)
        => Matcher term (TermF f ann term)
narrowF = do
  (Term (In ann syn)) <- target
  case project syn of
    Just fs -> pure (In ann fs)
    Nothing -> empty

-- | Matches on the head of the input list. Fails if the list is empty.
--
-- @mhead = only listToMaybe@
mhead :: Matcher [a] a
mhead = only listToMaybe

-- | Matches on 'Just' values.
--
-- @mjust = only id@
mjust :: Matcher (Maybe a) a
mjust = only id


-- | The entry point for executing matchers.
--   The Alternative parameter should be specialized by the calling context. If you want a single
--   result, specialize it to 'Maybe'; if you want a list of all terms and subterms matched by the
--   provided 'Matcher' action, specialize it to '[]'.
matchRecursively :: (Alternative m, Monad m, Corecursive t, Recursive t, Foldable (Base t))
                 => Matcher t a
                 -> t
                 -> m a
matchRecursively m = para (paraMatcher m)

paraMatcher :: (Alternative m, Monad m, Corecursive t, Foldable (Base t)) => Matcher t a -> RAlgebra (Base t) t (m a)
paraMatcher m t = matchOne (embedTerm t) m <|> foldMapA snd t

-- | Run one step of a 'Matcher' computation. Look at 'matchRecursively' if you want something
-- that folds over subterms.
matchOne :: (Alternative m, Monad m) => t -> Matcher t a -> m a
matchOne t (Choice a b) = matchOne t a <|> matchOne t b
matchOne t Target       = pure t
matchOne t (Match f m)  = foldMapA (`matchOne` m) (f t)
matchOne t (Comp g f)   = matchOne t f >>= \x -> matchOne x g
matchOne _ (Pure a)     = pure a
matchOne _ Empty        = empty
matchOne t (Then m f)   = matchOne t m >>= matchOne t . f
