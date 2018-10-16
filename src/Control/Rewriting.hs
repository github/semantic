{-# LANGUAGE GADTs, KindSignatures, LambdaCase, RankNTypes, ScopedTypeVariables, TypeOperators #-}

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
    RuleM
  , RewriteM
  , Rule
  , Rewrite
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
  , rewrite
  , rewriteM
  , runE
  ) where

import Prelude hiding (fail, id, (.))
import Prologue hiding (apply, try)

import           Control.Arrow
import           Control.Category
import           Control.Effect hiding (Local)
import           Data.Functor.Identity
import           Data.Profunctor
import qualified Data.Sum as Sum hiding (apply)
import           Data.Text (pack)

import Control.Abstract.Matching (Matcher, stepMatcher)
import Data.History as History
import Data.Term

-- | The fundamental type of rewriting rules. You can think of a
-- @RuleM env m from to@ as @env -> from -> m (Maybe to)@; in other words, a
-- Kleisli arrow with an immutable environment, supporting failure as
-- well as arbitrary effects in a monadic context. However, Rule
-- encompasses both 'cata' and 'para', so you can use them to fold
-- over 'Recursive' data types. Rules are covariant in their result
-- type and contravariant in their environment and input parameters.
data RuleM env (m :: * -> *) from to where
  Then  :: RuleM env m from a -> (a -> RuleM env m from b) -> RuleM env m from b
  Dimap :: (a -> b) -> (c -> d) -> RuleM env m b c -> RuleM env m a d
  Stop  :: String -> RuleM env m from to
  Alt   :: RuleM env m from to -> RuleM env m from to -> RuleM env m from to
  Pass  :: RuleM env m a a
  Comp  :: RuleM env m b c -> RuleM env m a b -> RuleM env m a c
  Split :: RuleM env m from to -> RuleM env m from' to' -> RuleM env m (from, from') (to, to')
  Fanin :: RuleM env m from to -> RuleM env m from' to -> RuleM env m (Either from from') to

  Context :: RuleM env m from env
  Local   :: (env' -> env) -> RuleM env m from to -> RuleM env' m from to
  Promote :: m to -> RuleM env m from to

  Recur :: ( Traversable f, Traversable g
           , old ~ Term f ann, new ~ Term g ann
           )
        => RuleM (env, old) m (f new) (g new)
        -> RuleM env m old new

  Somewhere :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs
               , f :< fs, g :< fs
               , term ~ Term (Sum fs) ann
               )
            => RuleM (env, term) m (f term) (g term)
            -> (term -> g term -> term)
            -> RuleM env m term term


-- | @a >>> b@ succeeds only if both @a@ and @b@ succeed. @id@ is the
-- identity rule that always succeeds; if you want to use it without
-- hiding Prelude's @id@, use 'target'.
instance Category (RuleM env m) where
  id  = Pass
  (.) = Comp

instance Functor (RuleM env m from) where
  fmap = rmap

instance Applicative (RuleM env m from) where
  pure  = arr . const
  (<*>) = ap

instance Monad (RuleM env m from) where
  (>>=) = Then

-- | This doesn't have a tremendous error message; you should
-- prefer 'fail'.
instance MonadPlus (RuleM env m from) where
  mzero = Stop "MonadPlus.mzero"

-- | The message passed to 'fail' will be shown to the user
-- in a 'TransformFailed' exception.
instance MonadFail (RuleM env m from) where
  fail = Stop

-- | @a <|> b@ succeeds if a or b succeeds.
instance Alternative (RuleM env m from) where
  (<|>) = Alt
  empty = Stop "Alternative.empty"

-- | You can map over the input type of a Rule contravariantly and the
-- output type covariantly, just like a function.
instance Profunctor (RuleM env m) where
  dimap = Dimap

-- | You can use arrow operations and syntax, if you're feeling saucy.
-- This also provides the useful '&&&' operator, which runs
-- two rules and returns a tuple of the results.
instance Arrow (RuleM env m) where
  arr f = Dimap id f id
  (***) = Split

-- | This instance lets you use @if@ and @case@ in arrow syntax over
-- rules, and provides '|||', which lifts its arguments into
-- a single rule that takes 'Either' values and dispatches appropriately.
instance ArrowChoice (RuleM env m) where
  f +++ g = (Left <$> f) ||| (Right <$> g)
  (|||)   = Fanin

-- | A 'RewriteM' is a 'RuleM' that does not change the type of its input.
type RewriteM env m item = RuleM env m item item

-- | 'Rule's and 'Rewrite's don't offer access to
-- their monad parameter.
type Rule env from to = forall m . RuleM env m from to
type Rewrite env item = Rule env item item

-- | Used to indicate failure and retrying.
-- TODO: look into using Data.Error.
newtype TransformFailed = TransformFailed Text deriving (Show, Eq)

--
-- Primitives defined in terms of 'Rule' constructors for
-- access to Rule-based state and effects.
--

-- | Extract the input parameter being considered by this rule.
-- An alias for 'id'.
target :: Rule env from from
target = id

-- | Extract the environment parameter within a rule.
context :: Rule env from env
context = Context

-- | Map a function over the environment parameter. Note that this is contravariant
-- rather than covariant, in contrast to 'Reader'.
localContext :: (newenv -> oldenv) -> Rule oldenv from to -> Rule newenv from to
localContext = Local

--
-- Building rules out of functions or monadic values.
--

-- | Builds a 'Rule' out of a function. Alias for 'arr'.
purely :: (from -> to) -> Rule env from to
purely = arr

-- | Promote a monadic value to a Rule in that monad. Analogous to
-- 'Control.Monad.Trans.lift', but 'RuleM' cannot be both an instance
-- of Category and of MonadTrans due to parameter order.
promote :: m to -> RuleM env m from to
promote = Promote

-- | Promote a 'Matcher' to a 'Rule'.
fromMatcher :: Matcher from to -> Rule env from to
fromMatcher m = target >>= \t -> maybeM (fail "fromMatcher") (stepMatcher t m)

-- | Promote a Rule from a recursive functor to one over terms, operating
-- leaf-to-root in the style of 'Data.Functor.Foldable.para'.
leafToRoot :: (Traversable f, Traversable g)
           => RuleM (env, Term f ann) m (f (Term g ann)) (g (Term g ann))
           -> RuleM env m (Term f ann) (Term g ann)
leafToRoot = Recur

--
-- General-purpose combinators
--

-- | Try applying a 'Rewrite', falling back on the identity rule if it fails.
--
-- @
--  try x = x <|> id
-- @
try :: Rewrite env term -> Rewrite env term
try = (<|> id)

-- | Feed one datum into a Rule. Similar to '&'.
apply :: Rule env x to -> x -> Rule env from to
apply rule x = pure x >>> rule

-- | The identity rule, but one that emits a trace of the provided
-- string as a side-effect. Useful for naming rules, as in
-- @
--  tracing "rule fired" >>> someRule >>> tracing "rule completed"
-- @
tracing :: (Member Trace sig, Carrier sig m, Functor m) => String -> RewriteM env m item
tracing s = id >>= (\t -> promote (t <$ trace s))

--
-- Combinators for operating on Terms and Sums
--

-- | Project from a 'Sum' to a component of that sum, failing
-- if the projection fails.
projecting :: (f :< fs) => Rule env (Sum fs recur) (f recur)
projecting = target >>= Sum.projectGuard

-- | Inject a component into a 'Sum'. This always succeeds.
injecting :: (f :< fs) => Rule env (f recur) (Sum fs recur)
injecting = arr Sum.inject

-- | Promote a Rule over the components of 'Sum' values to
-- 'Sum's themselves.
--
-- @
--  insideSum x = projecting >>> x >>> injecting
-- @
--
insideSum :: (f :< fs, g :< gs)
           => RuleM env m (f recur) (g recur)
           -> RuleM env m (Sum fs recur) (Sum gs recur)
insideSum x = projecting >>> x >>> injecting

--
-- Helpers for the leaf-to-root + insideSum idiom.
--

-- | Promote a 'Rule' over 'Sum' components everywhere inside a 'Term',
-- operating leaf-to-root and only succeeding if the 'Rule' can be applied everywhere.
everywhere :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs, f :< fs
              , Apply Functor gs, Apply Foldable gs, Apply Traversable gs, g :< gs
              )
           => RuleM (env, Term (Sum fs) ann) m (f (Term (Sum gs) ann)) (g (Term (Sum gs) ann))
           -> RuleM env m (Term (Sum fs) ann) (Term (Sum gs) ann)
everywhere = leafToRoot . insideSum

-- | @somewhere rule fn@, when applied to a 'Term' over 'Sum' values,
-- will recurse through the provided term in the style of a paramophism.
-- If the provided rule succeeds, the @fn@ function, which wraps the
-- result of the provided @rule@ back into a 'Term', is applied.
-- If at some stage the @rule@ does not succeed, no modifications
-- are made to that level (though they may affect the children
-- or parents of that level).
--
-- The finalizer function @fn@ is very often 'markRefactored', which ensures
-- that any term possessing a 'History' is marked as 'Refactored'.
somewhere :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs
              , f :< fs, g :< fs
              )
           => Rule (env, Term (Sum fs) ann) (f (Term (Sum fs) ann)) (g (Term (Sum fs) ann))
           -> (Term (Sum fs) ann -> g (Term (Sum fs) ann) -> Term (Sum fs) ann)
           -> Rewrite env (Term (Sum fs) ann)
somewhere = Somewhere


-- | As 'somewhere', but the wrapper is implicit, extracting the needed annotation
-- history at each level from the original level.
somewhere' :: ( Apply Functor fs, Apply Foldable fs, Apply Traversable fs
             , f :< fs, g :< fs
             )
          => Rule (env, Term (Sum fs) ann) (f (Term (Sum fs) ann)) (g (Term (Sum fs) ann))
          -> Rewrite env (Term (Sum fs) ann)
somewhere' = flip Somewhere (\x -> termIn (annotation x) . Sum.inject)


--
-- Helpers for termIn over a context and sum.
--

-- | Like termIn, except it uses the transform's context to get the current annotation.
-- Useful when a recursive rewrite rule has to add some subterms to a given term.
generate :: ( term ~ Term (Sum syn) ann
            , f :< syn
            )
         => f term -> Rule term a term
generate x = termIn <$> (termAnnotation <$> context) <*> pure (Sum.inject x)

-- | As 'generate', but operating in a tuple context of the sort you might see
-- in a 'leafToRoot' invocation.
generate' :: ( term ~ Term (Sum syn) ann
            , f :< syn
            )
         => f term -> Rule (env, term) a term
generate' x = termIn <$> (termAnnotation . snd <$> context) <*> pure (Sum.inject x)

-- | If we are operating in a History context, tag the provided sum
-- with a 'Refactored' annotation derived from the current context.
modified :: (Apply Functor syn, f :< syn, term ~ Term (Sum syn) History)
         => f term
         -> Rule (env, term) a term
modified x = History.remark Refactored <$> generate' x

-- | Mark the provided functor with a 'Refactored' version of the original
-- 'Term'. This is useful for passing in to 'somewhere''.
markRefactored :: (Apply Functor fs, g :< fs)
               => Term (Sum fs) History
               -> g (Term (Sum fs) History)
               -> Term (Sum fs) History
markRefactored old t = remark Refactored (termIn (annotation old) (inject t))


--
-- Interpreters
--

-- | Apply a transform in an monadic context.
rewriteM :: Monad m
         => RuleM env m from to
         -> env
         -> from
         -> m (Either TransformFailed to)
rewriteM r env from = runE env from r

-- | Apply a 'PureRule'.
rewrite :: RuleM env Identity from to
        -> env
        -> from
        -> Either TransformFailed to
rewrite r env from = runIdentity $ runE env from r

cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> (t -> m a)
cataM phi = c where c = phi <=< (traverse c . project)

paraM :: (Corecursive from, Recursive from, Traversable (Base from), Monad m) => (Base from (from, to) -> m to) -> (from -> m to)
paraM f = liftM snd . cataM run
  where run t = do
          a <- f t
          pure (embed $ fmap fst t, a)

eitherA :: Applicative f => (b -> f (Either a c)) -> Either a b -> f (Either a c)
eitherA = either (pure . Left)

-- | As 'rewriteM', but with some parameters reversed.
runE :: forall m env from to . Monad m
     => env
     -> from
     -> RuleM env m from to
     -> m (Either TransformFailed to)
runE env from = \case
  Then a f     -> runE env from a >>= eitherA (runE env from) . fmap f
  Dimap f g m  -> fmap (fmap g) (runE env (f from) m)
  Stop s       -> pure . Left . TransformFailed . pack $ s
  Pass         -> pure . pure $ from
  Promote p    -> fmap Right p

  Alt a b   -> runE env from a  >>= either (const (runE env from b)) (pure . Right)
  Fanin a b -> runE env from id >>= eitherA (rewriteM a env ||| rewriteM b env)
  Split a b -> runE env from id >>= eitherA (fmap bisequence . runKleisli prod)
    where prod = Kleisli (rewriteM a env) *** Kleisli (rewriteM b env)

  Local f m   -> runE (f env) from m
  Context     -> pure . pure $ env
  Comp a b    -> runE env from b >>= eitherA (rewriteM a env)

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
