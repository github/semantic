{-# LANGUAGE GADTs, KindSignatures, LambdaCase, ScopedTypeVariables, TypeOperators #-}

-- | This module provides 'Rule', a monadic DSL that abstracts the
-- details of rewriting a given datum into another type in some
-- effectful context. A term rewriter from @a@ to @b@ in some context
-- @m@ is similar in spirit to @a -> m b@ (the 'Kleisli' arrow), but
-- 'Rule' provides increased compositionality and generalizes well to
-- rewriting recursive structures at one or several levels, analogous
-- to 'cata' or 'para'. 'Rule's will be used both in refactoring (to
-- change properties of AST nodes) and in the reprinting pipeline (to
-- provide a configurable layout system).
--
-- Rules are composed with the 'Control.Category' methods, either
-- '>>>' for left-to-right composition or '<<<' for right-to-left.
-- They provide an 'Alternative' instance for choice.
--
-- Rewrite rules can be deterministic or nondeterministic, depending
-- on the inner monad by which they are parameterized. They support
-- failure and a form of try/catch without having to worry about the
-- details of exception handling.
module Control.Rewriting
  ( -- * Rule types
    Rule
  , Rewrite
  , PureRule
  , PureRewrite
  , TransformFailed (..)
  -- * Reexports from Control.Arrow
  , (>>>)
  , (<<<)
  , (&&&)
  , (|||)
  -- * Primitives
  , target
  , context
  , localContext
  -- * Building rules
  , purely
  , leafToRoot
  , promote
  , fromMatcher
  -- * General-purpose combinators
  , try
  , apply
  , tracing
  -- * Combinators for operating on terms and sums
  , projecting
  , injecting
  , insideSum
  -- * Helpers for transforming/rewriting tree-like structures.
  , everywhere
  , somewhere
  , somewhere'
  -- * Helpers for generating and annotating Term values
  , generate
  , generate'
  , modified
  , markRefactored
  -- * Running rules
  , applyEff
  , applyPure
  , runE
  ) where

import Prelude hiding (fail, id, (.))
import Prologue hiding (apply, try)

import           Control.Arrow
import           Control.Category
import           Control.Monad.Effect
import           Control.Monad.Effect.Trace
import           Data.Functor.Identity
import           Data.Profunctor
import           Data.Record
import qualified Data.Sum as Sum hiding (apply)
import           Data.Text (pack)

import Control.Abstract.Matching (Matcher, stepMatcher)
import Data.History as History
import Data.Term

-- | The fundamental type of rewriting rules. You can think of a @Rule
-- env m from to@ as @env -> from -> m (Maybe to)@; in other words, a
-- Kleisli arrow with an immutable environment, supporting failure as
-- well as arbitrary effects in a monadic context. However, Rule
-- encompasses both 'cata' and 'para', so you can use them to fold
-- over 'Recursive' data types. Rules are covariant in their result
-- type and contravariant in their environment and input parameters.
--
-- | Unlike KURE, a 'Rule' is a functor, applicative, monad, &c. with
-- no regard to its inner monad parameter.
data Rule env (m :: * -> *) from to where
  Then  :: Rule env m from a -> (a -> Rule env m from b) -> Rule env m from b
  Dimap :: (a -> b) -> (c -> d) -> Rule env m b c -> Rule env m a d
  Stop  :: String -> Rule env m from to
  Alt   :: Rule env m from to -> Rule env m from to -> Rule env m from to
  Pass  :: Rule env m a a
  Comp  :: Rule env m b c -> Rule env m a b -> Rule env m a c
  Split :: Rule env m from to -> Rule env m from' to' -> Rule env m (from, from') (to, to')
  Fanin :: Rule env m from to -> Rule env m from' to -> Rule env m (Either from from') to

  Context :: Rule env m from env
  Local   :: (env' -> env) -> Rule env m from to -> Rule env' m from to
  Promote :: m to -> Rule env m from to

  Recur :: (Traversable f, Traversable g)
        => Rule (env, Term f ann) m (f (Term g ann)) (g (Term g ann))
        -> Rule env m (Term f ann) (Term g ann)

  Somewhere :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs
               , f :< fs, g :< fs
               )
            => Rule (env, Term (Sum fs) ann) m (f (Term (Sum fs) ann)) (g (Term (Sum fs) ann))
            -> (Term (Sum fs) ann -> g (Term (Sum fs) ann) -> Term (Sum fs) ann)
            -> Rewrite env m (Term (Sum fs) ann)


-- | @a >>> b@ succeeds only if both @a@ and @b@ succeed. @id@ is the
-- identity rule that always succeeds; if you want to use it without
-- hiding Prelude's @id@, use 'target'.
instance Category (Rule env m) where
  id  = Pass
  (.) = Comp

instance Functor (Rule env m from) where
  fmap = rmap

instance Applicative (Rule env m from) where
  pure  = arr . const
  (<*>) = ap

instance Monad (Rule env m from) where
  (>>=) = Then

-- | This doesn't have a tremendous error message; you should
-- prefer 'fail'.
instance MonadPlus (Rule env m from) where
  mzero = Stop "MonadPlus.mzero"

-- | The message passed to 'fail' will be shown to the user
-- in a 'TransformFailed' exception.
instance MonadFail (Rule env m from) where
  fail = Stop

-- | @a <|> b@ succeeds if a or b succeeds.
instance Alternative (Rule env m from) where
  (<|>) = Alt
  empty = Stop "Alternative.empty"

-- | You can map over the input type of a Rule contravariantly and the
-- output type covariantly, just like a function.
instance Profunctor (Rule env m) where
  dimap = Dimap

-- | You can use arrow operations and syntax, if you're feeling saucy.
-- This also provides the useful '&&&' operator, which runs
-- two rules and returns a tuple of the results.
instance Arrow (Rule env m) where
  arr f = Dimap id f id
  (***) = Split

-- | This instance lets you use @if@ and @case@ in arrow syntax over
-- rules, and provides '|||', which lifts its arguments into
-- a single rule that takes 'Either' values and dispatches appropriately.
instance ArrowChoice (Rule env m) where
  f +++ g = (Left <$> f) ||| (Right <$> g)
  (|||)   = Fanin

-- | A 'Rewrite' is a 'Rule' which does not change the type of its
-- input. TODO: can we get more expressive than a type synonym?
-- newtypes? phantom types?
type Rewrite env m item = Rule env m item item

-- | 'PureRule's and 'PureRewrite's don't depend on any monadic effects,
-- save for catch/throw internally to handle failure.
type PureRule env from to = Rule env Identity from to
type PureRewrite env item = PureRule env item item

-- | Used to indicate failure and retrying.
-- TODO: look into using Data.Error.
newtype TransformFailed = TransformFailed Text deriving (Show, Eq)

--
-- Primitives defined in terms of 'Rule' constructors for
-- access to Rule-based state and effects.
--

-- | Extract the input parameter being considered by this rule.
-- An alias for 'id'.
target :: Rule env m from from
target = id

-- | Extract the environment parameter within a rule.
context :: Rule env m from env
context = Context

-- | Map a function over the environment parameter. Note that this is contravariant
-- rather than covariant, in contrast to 'Reader'.
localContext :: (newenv -> oldenv) -> Rule oldenv m from to -> Rule newenv m from to
localContext = Local

--
-- Building rules out of functions or monadic values.
--

-- | Builds a 'Rule' out of a function. Alias for 'arr'.
purely :: (from -> to) -> Rule env m from to
purely = arr

-- | Promote a monadic value to a Rule in that monad.
promote :: m to -> Rule env m from to
promote = Promote

-- | Promote a 'Matcher' to a 'Rule'.
fromMatcher :: Matcher from to -> Rule env m from to
fromMatcher m = target >>= \t -> maybeM (fail "fromMatcher") (stepMatcher t m)

-- | Promote a Rule from a recursive functor to one over terms, operating
-- leaf-to-root in the style of 'Data.Functor.Foldable.para'.
leafToRoot :: (Traversable f, Traversable g)
           => Rule (env, Term f ann) m (f (Term g ann)) (g (Term g ann))
           -> Rule env m (Term f ann) (Term g ann)
leafToRoot = Recur

--
-- General-purpose combinators
--

-- | Try applying a 'Rewrite', falling back on the identity rule if it fails.
--
-- @
--  try x = x <|> id
-- @
try :: Rewrite env m term -> Rewrite env m term
try = (<|> id)

-- | Apply a Rule to one datum. Similar to '&'.
apply :: Rule env m x to -> x -> Rule env m from to
apply rule x = pure x >>> rule

-- | The identity rule, but one that emits a trace of the provided
-- string as a side-effect. Useful for naming rules, as in
-- @
--  tracing "rule fired" >>> someRule >>> tracing "rule completed"
-- @
tracing :: Member Trace effs => String -> Rewrite env (Eff effs) item
tracing s = id >>= (\t -> promote (t <$ trace s))

--
-- Combinators for operating on Terms and Sums
--

-- | Project from a 'Sum' to a component of that sum, failing
-- if the projection fails.
projecting :: (f :< fs) => Rule env m (Sum fs recur) (f recur)
projecting = target >>= Sum.projectGuard

-- | Inject a component into a 'Sum'. This always succeeds.
injecting :: (f :< fs) => Rule env m (f recur) (Sum fs recur)
injecting = arr Sum.inject

-- | Promote a Rule over the components of 'Sum' values to
-- 'Sum's themselves.
--
-- @
--  insideSum x = projecting >>> x >>> injecting
-- @
--
insideSum :: (f :< fs, g :< gs)
           => Rule env m (f recur) (g recur)
           -> Rule env m (Sum fs recur) (Sum gs recur)
insideSum x = projecting >>> x >>> injecting

--
-- Helpers for the leaf-to-root + insideSum idiom.
--

-- | Promote a 'Rule' over 'Sum' components everywhere inside a 'Term',
-- operating leaf-to-root and only succeeding if the 'Rule' can be applied everywhere.
everywhere :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs, f :< fs
              , Apply Functor gs, Apply Foldable gs, Apply Traversable gs, g :< gs
              )
           => Rule (env, Term (Sum fs) ann) m (f (Term (Sum gs) ann)) (g (Term (Sum gs) ann))
           -> Rule env m (Term (Sum fs) ann) (Term (Sum gs) ann)
everywhere = leafToRoot . insideSum

-- | Given a 'Rule' over an @f@ and @g@ both in @fs@, promote that
-- rule to a 'Rewrite' over @fs@, applying said rule everywhere
-- possible. The resulting 'Rewrite' always succeeds.
somewhere :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs
             , f :< fs, g :< fs
             )
          => Rule (env, Term (Sum fs) ann) m (f (Term (Sum fs) ann)) (g (Term (Sum fs) ann))
          -> Rewrite env m (Term (Sum fs) ann)
somewhere = flip Somewhere (\x -> termIn (annotation x) . Sum.inject)

-- | As 'somewhere', but @somewhere' rule fn@ takes an extra @fn@
-- parameter used to inject a member back into the resulting 'Term'
-- type if the provided @rule@ succeeds. This will very often be
-- 'markRefactored', which toggles the annotation from 'Unmodified' to
-- 'Refactored'.
somewhere' :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs
              , f :< fs, g :< fs
              )
           => Rule (env, Term (Sum fs) ann) m (f (Term (Sum fs) ann)) (g (Term (Sum fs) ann))
           -> (Term (Sum fs) ann -> g (Term (Sum fs) ann) -> Term (Sum fs) ann)
           -> Rewrite env m (Term (Sum fs) ann)
somewhere' = Somewhere

--
-- Helpers for termIn over a context and sum.
--

-- | Like termIn, except it uses the transform's context to get the current annotation.
-- Useful when a recursive rewrite rule has to add some subterms to a given term.
generate :: ( term ~ Term (Sum syn) ann
            , f :< syn
            )
         => f term -> Rule term m a term
generate x = termIn <$> (termAnnotation <$> context) <*> pure (Sum.inject x)

-- | As 'generate', but operating in a tuple context of the sort you might see
-- in a 'leafToRoot' invocation.
generate' :: ( term ~ Term (Sum syn) ann
            , f :< syn
            )
         => f term -> Rule (env, term) m a term
generate' x = termIn <$> (termAnnotation . snd <$> context) <*> pure (Sum.inject x)

-- | If we are operating in a History context, tag the provided sum
-- with a 'Refactored' annotation derived from the current context.
modified :: (Apply Functor syn, f :< syn, term ~ Term (Sum syn) (Record (History : fields)))
         => f term -> Rule (env, term) m a term
modified x = History.remark Refactored <$> generate' x

-- | Mark the provided functor with a 'Refactored' version of the original
-- 'Term'. This is useful for passing in to 'somewhere''.
markRefactored :: (Apply Functor fs, g :< fs)
               => Term (Sum fs) (Record (History ': fields))
               -> g (Term (Sum fs) (Record (History ': fields)))
               -> Term (Sum fs) (Record (History ': fields))
markRefactored old t = remark Refactored (termIn (annotation old) (inject t))


--
-- Interpreters
--

-- | Apply a transform in an effectful context.
applyEff :: Monad m
         => Rule env m from to
         -> env
         -> from
         -> m (Either TransformFailed to)
applyEff r env from = runE env from r

-- | Apply a 'PureRule'.
applyPure :: PureRule env from to
          -> env
          -> from
          -> Either TransformFailed to
applyPure r env from = runIdentity $ runE env from r

cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> (t -> m a)
cataM phi = c where c = phi <=< (traverse c . project)

paraM :: (Corecursive from, Recursive from, Traversable (Base from), Monad m) => (Base from (from, to) -> m to) -> (from -> m to)
paraM f = liftM snd . cataM run
  where run t = do
          a <- f t
          pure (embed $ fmap fst t, a)

eitherA :: Applicative f => (b -> f (Either a c)) -> Either a b -> f (Either a c)
eitherA = either (pure . Left)

-- | As 'applyEff', but with some parameters reversed.
runE :: forall m env from to . Monad m
     => env
     -> from
     -> Rule env m from to
     -> m (Either TransformFailed to)
runE env from = \case
  Then a f     -> runE env from a >>= eitherA (runE env from) . fmap f
  Dimap f g m  -> fmap (fmap g) (runE env (f from) m)
  Stop s       -> pure . Left . TransformFailed . pack $ s
  Pass         -> pure . pure $ from
  Promote p    -> fmap Right p

  Alt a b   -> runE env from a  >>= either (const (runE env from b)) (pure . Right)
  Fanin a b -> runE env from id >>= eitherA (applyEff a env ||| applyEff b env)
  Split a b -> runE env from id >>= eitherA (fmap bisequence . runKleisli prod)
    where prod = Kleisli (applyEff a env) *** Kleisli (applyEff b env)

  Local f m   -> runE (f env) from m
  Context     -> pure . pure $ env
  Comp a b    -> runE env from b >>= eitherA (applyEff a env)

  Recur r -> paraM go from where
    go (In ann recur) = eitherA hi (traverse snd recur)
      where hi x = fmap (fmap (termIn ann)) (runE (env, termIn ann (fmap fst recur)) x r)

  Somewhere r pin -> paraM go from where
     go arg = let
         orig = Term $ fmap fst arg
         xformed = fmap Term (traverse snd arg)
       in xformed & either (const (pure (Right orig))) (\given ->
            -- We could pull out this projectTerm into a field on Somewhere itself,
            -- but that seems like overkill for the time being.
            projectTerm given & maybe (pure (Right given)) (\asSum ->
              Right . either (const given) (pin given)
                <$> runE (env, orig) asSum r))
