{-# LANGUAGE ApplicativeDo, GADTs, RankNTypes, TypeOperators, UndecidableInstances, LambdaCase #-}

module Reprinting.Tokenize
  ( module Data.Reprinting.Token
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

import Prelude hiding (fail, log)
import Prologue hiding (hash, Element)

import Data.History
import Data.List (intersperse)
import Data.Range
import Data.Record
import Data.Reprinting.Token
import qualified Data.Machine as Machine
import Data.Source
import Data.Term

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

compile :: State -> Tokenizer a -> Machine.PlanT k Token m (State, a)
compile p = \case
  Pure a   -> pure (p, a)
  Bind a f -> compile p a >>= (\(new, v) -> compile new (f v))
  Tell t   -> Machine.yield t *> pure (p, ())
  Get      -> pure (p, p)
  Put p'   -> pure (p', ())

instance Functor Tokenizer where
  fmap = liftA

instance Applicative Tokenizer where
  pure  = Pure
  (<*>) = ap

instance Monad Tokenizer where
  (>>=) = Bind

-- Internal state. This is hidden within Tokenizer rather
-- than being exposed to the outside by reifying this in
-- an Eff State. If we encounter performance problems,
-- we can trade off the slightly more pleasant interface
-- that a deep embedding gets us.

data Strategy
  = Reprinting
  | PrettyPrinting
    deriving (Eq, Show)

data State = State
  { _source   :: Source
  , _history  :: History
  , _strategy :: Strategy
  , _cursor   :: Int
  , _enabled  :: Bool
  } deriving (Show, Eq)


-- Builtins

-- | Yield an 'Element' token in a 'Tokenizer' context.
yield :: Element -> Tokenizer ()
yield e = do
  on <- _enabled <$> Get
  when on . Tell . TElement $ e

-- | Yield a 'Control' token.
control :: Control -> Tokenizer ()
control = Tell . TControl

-- | Yield a 'Chunk' of some 'Source'.
chunk :: Source -> Tokenizer ()
chunk = Tell . Chunk

finish :: Tokenizer ()
finish = do
  crs <- asks _cursor
  log ("Finishing, cursor is " <> show crs)
  src <- asks _source
  chunk (dropSource crs src)

-- State handling

asks :: (State -> a) -> Tokenizer a
asks f = f <$> Get

modify :: (State -> State) -> Tokenizer ()
modify f = Get >>= \x -> Put . f $! x

enable, disable :: Tokenizer ()
enable  = modify (\x -> x { _enabled = True })
disable = modify (\x -> x { _enabled = False})

move :: Int -> Tokenizer ()
move c = modify (\x -> x { _cursor = c })

withHistory :: (Annotated t (Record fields), HasField fields History)
            => t
            -> Tokenizer a
            -> Tokenizer a
withHistory t act = do
  old <- asks _history
  modify (\x -> x { _history = getField (annotation t)})
  act <* modify (\x -> x { _history = old })

withStrategy :: Strategy -> Tokenizer a -> Tokenizer a
withStrategy s act = do
  old <- Get
  Put (old { _strategy = s })
  res <- act
  new <- Get
  Put (new { _strategy = _strategy old })
  pure res

-- The reprinting algorithm.

-- | A subterm algebra inspired by the /Scrap Your Reprinter/ algorithm.
descend :: (Tokenize constr, HasField fields History) => SubtermAlgebra constr (Term a (Record fields)) (Tokenizer ())
descend t = do
  (State src hist strat crs _) <- asks id
  let into s = withHistory (subterm s) (subtermRef s)
  case (hist, strat) of
    (Unmodified _, _) -> do
      tokenize (fmap into t)
      disable
    (Refactored _, PrettyPrinting) -> do
      enable
      tokenize (fmap into t)
    (Refactored r, Reprinting) -> do
      enable
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
within :: Context -> Tokenizer () -> Tokenizer ()
within c r = control (Enter c) *> r <* control (Exit c)

-- | Like 'within', but adds 'TOpen' and 'TClose' elements around the action.
within' :: Context -> Tokenizer () -> Tokenizer ()
within' c x = within c $ yield TOpen *> x <* yield TClose

-- | Emit a sequence of tokens interspersed with 'TSep'.
sep :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sep = intersperse (yield TSep) . toList

-- | Emit a sequence of tokens each with trailing 'TSep'.
sepTrailing :: Foldable t => t (Tokenizer ()) -> [Tokenizer ()]
sepTrailing = foldr (\x acc -> x : yield TSep : acc) mempty

-- | Emit a sequence of tokens within a 'TList' Context with appropriate 'TOpen',
-- 'TClose' tokens surrounding.
list :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
list = within' TList . sequenceA_ . sep

-- | Emit a sequence of tokens within a 'THash' Context with appropriate
-- 'TOpen', 'TClose' tokens surrounding and interspersing 'TSep'.
hash :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
hash = within' THash . sequenceA_ . sep

-- | Emit key value tokens with a 'TSep' within an TPair Context
pair :: Tokenizer () -> Tokenizer () -> Tokenizer ()
pair k v = within TPair $ k *> yield TSep <* v

-- | Emit a sequence of tokens within an Imperative Context with appropriate
-- 'TOpen', 'TClose' tokens surrounding and interspersing 'TSep'.
imperative :: Foldable t => t (Tokenizer ()) -> Tokenizer ()
imperative = within' Imperative . sequenceA_ . sep

-- | Shortcut for @const (pure ())@, useful for when no action
-- should be taken.
ignore :: a -> Tokenizer ()
ignore = const (pure ())

-- | An instance of the 'Tokenize' typeclass describes how to emit tokens to
-- pretty print the value of the supplied constructor in its AST context.
class (Show1 constr, Traversable constr) => Tokenize constr where
  -- | Should emit control and data tokens.
  tokenize :: FAlgebra constr (Tokenizer ())

tokenizing :: (Show (Record fields), Tokenize a, HasField fields History)
           => Source
           -> Term a (Record fields)
           -> Machine.Source Token
tokenizing src term = pipe
  where pipe  = Machine.construct . fmap snd $ compile state go
        state = State src (getField (termAnnotation term)) Reprinting 0 False
        go    = disable *> foldSubterms descend term <* finish

-- | Sums of reprintable terms are reprintable.
instance (Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Tokenize fs) => Tokenize (Sum fs) where
  tokenize = apply @Tokenize tokenize

-- | Annotated terms are reprintable and operate in a context derived from the annotation.
instance (HasField fields History, Show (Record fields), Tokenize a) => Tokenize (TermF a (Record fields)) where
  tokenize t = withHistory t (tokenize (termFOut t))

instance Tokenize [] where
  tokenize = imperative
