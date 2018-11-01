{-# LANGUAGE GADTs, LambdaCase, RankNTypes, UndecidableInstances #-}

module Reprinting.Tokenize
  ( module Token
  , module Scope
  , module Operator
  , History (..)
  , mark
  , remark
    -- * The Reprinter monad
  , Tokenizer
  , yield
  , control
  , within
  , within'
  , log
  , ignore
  , sep
  , sepTrailing
  , list
  , hash
  , pair
  , imperative
  -- * Tokenize interface
  , Tokenize (..)
  -- * Invocation/results
  , tokenizing
  ) where

import Prelude hiding (fail, log, filter)
import Prologue hiding (Element, hash)

import           Data.History
import           Data.List (intersperse)
import qualified Data.Machine as Machine
import           Data.Range
import           Data.Reprinting.Scope (Scope)
import qualified Data.Reprinting.Scope as Scope
import           Data.Reprinting.Token as Token
import           Data.Reprinting.Operator as Operator
import           Data.Source
import           Data.Term

-- | The 'Tokenizer' monad represents a context in which 'Control'
-- tokens and 'Element' tokens can be sent to some downstream
-- consumer. Its primary interface is through the 'Tokenize'
-- typeclass, and is compiled to a 'Data.Machine.Source' by
-- 'tokenizing'.
data Tokenizer a where
  Pure :: a -> Tokenizer a
  Bind :: Tokenizer a -> (a -> Tokenizer b) -> Tokenizer b

  Tell :: Token -> Tokenizer ()

  Get :: Tokenizer State
  Put :: State -> Tokenizer ()

-- Tokenizers are compiled into a Plan capable of being converted
-- to a Source. Note that the state parameter is internal to the
-- tokenizer being run: the invoker of 'tokenizing' doesn't need
-- to keep track of it at all.
compile :: State -> Tokenizer a -> Machine.Plan k Token (State, a)
compile p = \case
  Pure a   -> pure (p, a)
  Bind a f -> compile p a >>= (\(new, v) -> compile new (f v))
  Tell t   -> Machine.yield t $> (p, ())
  Get      -> pure (p, p)
  Put p'   -> pure (p', ())

instance Functor Tokenizer where fmap = liftA

instance Applicative Tokenizer where
  pure  = Pure
  (<*>) = ap

instance Monad Tokenizer where (>>=) = Bind

data Strategy
  = Reprinting
  | PrettyPrinting
    deriving (Eq, Show)

data Filter
  = AllowAll
  | ForbidData
    deriving (Eq, Show)

data State = State
  { source   :: Source   -- We need to be able to slice
  , history  :: History  -- What's the history of the term we're examining
  , strategy :: Strategy -- What are we doing right now?
  , cursor   :: Int      -- Where do we begin slices?
  , filter   :: Filter   -- Should we ignore data tokens?
  } deriving (Show, Eq)

-- Builtins

-- | Yield an 'Element' token in a 'Tokenizer' context.
yield :: Element -> Tokenizer ()
yield e = do
  on <- filter <$> Get
  when (on == AllowAll) . Tell . Element $ e

-- | Yield a 'Control' token.
control :: Control -> Tokenizer ()
control = Tell . Control

-- | Yield a 'Chunk' of some 'Source'.
chunk :: Source -> Tokenizer ()
chunk = Tell . Chunk

-- | Ensures that the final chunk is emitted
finish :: Tokenizer ()
finish = do
  crs <- asks cursor
  log ("Finishing, cursor is " <> show crs)
  src <- asks source
  chunk (dropSource crs src)

-- State handling

asks :: (State -> a) -> Tokenizer a
asks f = f <$> Get

modify :: (State -> State) -> Tokenizer ()
modify f = Get >>= \x -> Put . f $! x

allowAll, forbidData :: Tokenizer ()
allowAll   = modify (\x -> x { filter = AllowAll })
forbidData = modify (\x -> x { filter = ForbidData })

move :: Int -> Tokenizer ()
move c = modify (\x -> x { cursor = c })

withHistory :: Annotated t History
            => t
            -> Tokenizer a
            -> Tokenizer a
withHistory t act = do
  old <- asks history
  modify (\x -> x { history = annotation t })
  act <* modify (\x -> x { history = old })

withStrategy :: Strategy -> Tokenizer a -> Tokenizer a
withStrategy s act = do
  old <- Get
  Put (old { strategy = s })
  res <- act
  new <- Get
  Put (new { strategy = strategy old })
  pure res

-- The reprinting algorithm.

-- | A subterm algebra inspired by the /Scrap Your Reprinter/ algorithm.
descend :: Tokenize constr => SubtermAlgebra constr (Term a History) (Tokenizer ())
descend t = do
  (State src hist strat crs _) <- asks id
  let into s = withHistory (subterm s) (subtermRef s)
  case (hist, strat) of
    (Unmodified _, _) -> do
      tokenize (fmap into t)
      forbidData
    (Refactored _, PrettyPrinting) -> do
      allowAll
      tokenize (fmap into t)
    (Refactored r, Reprinting) -> do
      allowAll
      let delimiter = Range crs (start r)
      unless (delimiter == Range 0 0) $ do
        log ("slicing: " <> show delimiter)
        chunk (slice delimiter src)
      move (start r)
      tokenize (fmap (withStrategy PrettyPrinting . into) t)
      move (end r)


-- Combinators

-- | Emit a log message to the token stream. Useful for debugging.
log :: String -> Tokenizer ()
log = control . Log

-- | Emit an Enter for the given context, then run the provided
-- action, then emit a corresponding Exit.
within :: Scope -> Tokenizer () -> Tokenizer ()
within c r = control (Enter c) *> r <* control (Exit c)

-- | Like 'within', but adds 'Open' and 'Close' elements around the action.
within' :: Scope -> Tokenizer () -> Tokenizer ()
within' c x = within c $ yield Token.Open *> x <* yield Token.Close

-- | Emit a sequence of tokens interspersed with 'Sep'.
sep :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sep = intersperse (yield Token.Sep) . toList

-- | Emit a sequence of tokens each with trailing 'Sep'.
sepTrailing :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sepTrailing = foldr (\x acc -> x : yield Token.Sep : acc) mempty

-- | Emit a sequence of tokens within a 'List' Scope with appropriate 'Open',
-- 'TClose' tokens surrounding.
list :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
list = within' Scope.List . sequenceA_ . sep

-- | Emit a sequence of tokens within a 'Hash' Scope with appropriate
-- 'Open', 'TClose' tokens surrounding and interspersing 'Sep'.
hash :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
hash = within' Scope.Hash . sequenceA_ . sep

-- | Emit key value tokens with a 'Sep' within a scoped 'Pair'.
pair :: Tokenizer () -> Tokenizer () -> Tokenizer ()
pair k v = within Scope.Pair $ k *> yield Token.Sep <* v

-- | Emit a sequence of tokens within an 'Imperative' scope with
-- appropriate 'Open', 'Close' tokens surrounding and interspersing
-- 'Sep'.
imperative :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
imperative = within' Scope.Imperative . sequenceA_ . sep

-- | Shortcut for @const (pure ())@, useful for when no action
-- should be taken.
ignore :: a -> Tokenizer ()
ignore = const (pure ())

-- | An instance of the 'Tokenize' typeclass describes how to emit tokens to
-- pretty print the value of the supplied constructor in its AST context.
class (Show1 constr, Traversable constr) => Tokenize constr where
  -- | Should emit control and data tokens.
  tokenize :: FAlgebra constr (Tokenizer ())

tokenizing :: Tokenize a
           => Source
           -> Term a History
           -> Machine.Source Token
tokenizing src term = pipe
  where pipe  = Machine.construct . fmap snd $ compile state go
        state = State src (termAnnotation term) Reprinting 0 ForbidData
        go    = forbidData *> foldSubterms descend term <* finish

-- | Sums of reprintable terms are reprintable.
instance (Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Tokenize fs) => Tokenize (Sum fs) where
  tokenize = apply @Tokenize tokenize

-- | Annotated terms are reprintable and operate in a context derived from the annotation.
instance Tokenize a => Tokenize (TermF a History) where
  tokenize t = withHistory t (tokenize (termFOut t))

instance Tokenize [] where
  tokenize = imperative
