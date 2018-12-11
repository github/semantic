{-# LANGUAGE GADTs, TypeOperators #-}

module Control.Rewriting
  ( -- | Core types
    Rewrite
  , Rule
  -- | Combinators
  , target
  , purely
  , ensure
  , try
  , (>+>)
  -- | Reexports from Control.Category
  , (>>>)
  , (<<<)
  , (^>>)
  -- | Predicate filtering
  , refine
  , only
  -- | Projecting terms and sums
  , narrow
  , narrowF
  , enter
  , create
  -- | Useful rewrites
  , mhead
  , mjust
  -- | Running rewrites
  , rewrite
  , recursively
  -- | Strategic rewrite combinators
  , applyAll
  , applyAny
  , topDownAll
  , topDownAny
  ) where

import Prelude hiding (id, (.))
import Prologue hiding (First, project, try)

import           Control.Arrow
import           Control.Category
import qualified Data.Functor.Foldable as Foldable

import Data.History
import Data.Sum
import Data.Term

-- | A @Rewrite t a@ is a parser over some 'Recursive' and
--   'Corecursive' type @t@, yielding values of type @a@.
--
-- Matching operations are implicitly recursive: when you run a
-- 'Rewrite', it is applied bottom-up. If a matching operation
-- returns a value, it is assumed to have succeeded. You use the
-- 'guard', 'narrow', and 'ensure' functions to control whether a
-- given datum is matched. The @t@ datum matched by a matcher is
-- immutable; if you need to modify values during a match operation,
-- consider using Control.Rewriting.
data Rewrite t a where
  -- TODO: Choice is inflexible and slow. A Sum over fs can be queried for its index, and we can build a jump table over that.
  -- We can copy NonDet to have fair conjunction or disjunction.
  Choice :: Rewrite t a -> Rewrite t a -> Rewrite t a
  Target :: Rewrite t t
  Empty  :: Rewrite t a
  Comp   :: Rewrite b c -> Rewrite a b -> Rewrite a c
  Split  :: Rewrite b c -> Rewrite b' c' -> Rewrite (b, b') (c, c')
  -- We could have implemented this by changing the semantics of how Then is interpreted, but that would make Then and Sequence inconsistent.
  Match  :: (t -> Maybe u) -> Rewrite u a -> Rewrite t a
  Pure   :: a -> Rewrite t a
  Then   :: Rewrite t b -> (b -> Rewrite t a) -> Rewrite t a

-- | A rewriter is a matcher that doesn't vary in its input and output types.
type Rule t = Rewrite t t

instance Functor (Rewrite t) where
  fmap = liftA

instance Applicative (Rewrite t) where
  pure   = Pure
  -- We can add a Sequence constructor to optimize this when we need.
  (<*>)  = ap

instance Alternative (Rewrite t) where
  empty = Empty
  (<|>) = Choice

instance Monad (Rewrite t) where
  (>>=) = Then

-- | Rewrites are generally composed left-to-right with '>>>'.
instance Category Rewrite where
  id  = Target
  (.) = Comp

instance Arrow Rewrite where
  (***) = Split
  arr f = fmap f target

-- | 'target' extracts the 't' that a given 'Rewrite' is operating upon.
--   Similar to a reader monad's 'ask' function. This is an alias for 'id'
target :: Rewrite t t
target = id

-- | 'ensure' succeeds iff the provided predicate function returns true when applied to the matcher's 'target'.
-- If it succeeds, it returns the matcher's 'target'.
ensure :: (t -> Bool) -> Rewrite t t
ensure f = target >>= \c -> c <$ guard (f c)

-- | Promote a pure function to a 'Rewrite'. An alias for 'arr'.
purely :: (a -> b) -> Rewrite a b
purely = arr

-- | 'refine' takes a modification function and a new matcher action
-- the target parameter of which is the result of the modification
-- function. If the modification function returns 'Just' when applied
-- to the current 'target', the given matcher is executed with the
-- result of that 'Just' as the new target; if 'Nothing' is returned,
-- the action fails.
--
-- This is the lowest-level combinator for applying a predicate function
-- to a matcher. In practice, you'll generally use the 'enter' and 'narrow'
-- combinators to iterate on recursive 'Term' values.
refine :: (t -> Maybe u) -> Rewrite u a -> Rewrite t a
refine = Match

-- | An alias for the common pattern of @match f id@.
only :: (t -> Maybe u) -> Rewrite t u
only f = Match f Target

-- | @a >+> b@ performs @a@ and @b@ in sequence, succeeding if one
-- or both succeed.
infixr 1 >+>
(>+>) :: Rule t -> Rule t -> Rule t
a >+> b = (a >>> b) <|> a <|> b

try :: Rule a -> Rule a
try a = a <|> id


-- | The 'enter' combinator is the primary interface for creating
-- matchers that 'project' their internal 'Term' values into some
-- constituent type. Given a function from a constituent type @f@
-- @need p@ succeeds if the provided term can be projected into
-- an @f@, then applies the @p@ function.
enter :: ( f :< fs
         , term ~ Term (Sum fs) ann
         )
     => (f term -> b)
     -> Rewrite term b
enter f = Match (fmap f . projectTerm) target

-- | 'narrow' projects the given 'Term' of 'Sum's into a constituent member
-- of that 'Sum', failing if the target cannot be thus projected.
narrow :: (f :< fs) => Rewrite (Term (Sum fs) ann) (f (Term (Sum fs) ann))
narrow = purely projectTerm >>= foldMapA pure

-- | Like 'narrow', but it returns the result of the projection in a
-- 'TermF'. Useful for returning a matched node after ensuring its
-- contents are projectable and valid, e.g @narrowF <* a >:: b >>>
-- ensure f@, without losing valuable annotation info.
narrowF :: (f :< fs, term ~ Term (Sum fs) ann)
        => Rewrite term (TermF f ann term)
narrowF = do
  (Term (In ann syn)) <- target
  case project syn of
    Just fs -> pure (In ann fs)
    Nothing -> empty

create :: (f :< fs, Apply Functor fs, term ~ Term (Sum fs) History) => f term -> Rule term
create f = remark Refactored <$> (injectTerm <$> fmap annotation id <*> pure f)

-- | Matches on the head of the input list. Fails if the list is empty.
--
-- @mhead = only listToMaybe@
mhead :: Rewrite [a] a
mhead = only listToMaybe

-- | Matches on 'Just' values.
--
-- @mjust = only id@
mjust :: Rewrite (Maybe a) a
mjust = only id

-- | Run one step of a 'Rewrite' computation. Look at 'rewriteRecursively' if you want something
-- that folds over subterms.
rewrite :: (Alternative m, Monad m) => t -> Rewrite t a -> m a
rewrite t (Choice a b) = rewrite t a <|> rewrite t b
rewrite t Target       = pure t
rewrite t (Match f m)  = foldMapA (`rewrite` m) (f t)
rewrite t (Comp g f)   = rewrite t f >>= \x -> rewrite x g
rewrite _ (Pure a)     = pure a
rewrite _ Empty        = empty
rewrite t (Then m f)   = rewrite t m >>= rewrite t . f
rewrite t (Split f g)  = rewrite t id >>= \(a, b) -> (,) <$> rewrite a f <*> rewrite b g

-- | Run a 'Rewrite' over a 'Recursive' data structure, leaf-to-root. Unlike recursion with
-- the Lämmel combinators, this allows you to use a 'Rewrite' rather than just a 'Rule',
-- allowing for the returned type to change. If you pass in a 'Rule' here, consider whether
-- the strategic combinators are actually what you want.
recursively :: (Alternative m, Monad m, Corecursive t, Recursive t, Foldable (Base t))
            => Rewrite t a
            -> t
            -> m a
recursively m = para (paraRewrite m)

paraRewrite :: (Alternative m, Monad m, Corecursive t, Foldable (Base t)) => Rewrite t a -> RAlgebra (Base t) t (m a)
paraRewrite m t = rewrite (embedTerm t) m <|> foldMapA snd t

-- * For more information about these combinators, see /The Essence of Strategic Programming/
-- (Lämmel, Visser, and Visser, 2003).

-- | Runs a 'Rule' one level deep in the provided 'Recursive' type, succeeding
-- if all children succeed.
applyAll :: (Corecursive t, Recursive t, Traversable (Base t))
         => Rule t -> Rule t
applyAll m = id >>= \t -> Foldable.embed <$> traverse go (Foldable.project t)
  where go a = pure a >>> m

-- | Runs a 'Rule' one level deep, succeeding if any children succeed.
applyAny :: (Corecursive t, Recursive t, Traversable (Base t))
         => Rule t -> Rule t
applyAny m = applyAll (m <|> id)

-- | Runs a 'Rule' top-down over a 'Recursive' type, succeeding if
-- all subsequent rewrites succeed.
topDownAll :: (Corecursive t, Recursive t, Traversable (Base t))
           => Rule t -> Rule t
topDownAll r = let go = r >>> applyAll go in go

-- | Runs a 'Rule' top-down over a 'Recursive' type, succeeding if
-- any rewrites succeed.
topDownAny :: (Corecursive t, Recursive t, Traversable (Base t))
           => Rule t -> Rule t
topDownAny r = let go = r >+> applyAny go in go
