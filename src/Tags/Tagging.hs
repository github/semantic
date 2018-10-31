{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Tagging
( runTagging
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Analysis.ConstructorName
import           Control.Matching hiding (target)
import           Control.Arrow hiding (first)
import           Control.Effect as Eff
import           Control.Effect.Error as Error
import qualified Control.Effect.State as State
import           Control.Monad
import           Control.Monad.Trans
import           Control.Rewriting hiding (apply)
import           Data.Abstract.Declarations
import           Data.Abstract.Name
import           Data.Blob
import           Data.History
import           Data.List (intersperse)
import           Data.Location
import           Data.Machine as Machine
import           Data.Range
import qualified Data.Source as Source
import           Data.Span
import           Data.Term
import           Data.Text hiding (empty)

import Tags.Taggable
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module"]

data Symbol
  = Symbol
  { name :: Text
  , kind :: Text
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  }
  deriving (Eq, Show)

runTagging ::
  ( Apply Functor fs
  , Apply Foldable fs
  , Apply Traversable fs
  , Apply Show1 fs
  , Apply Taggable fs
  , Apply ConstructorName fs
  , Apply Declarations1 fs
  , [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  )
  => Blob
  -> Term (Sum fs) Location
  -> Either TranslationError [Symbol]
runTagging blob tree
  = Eff.run
  . Error.runError
  . fmap snd
  . State.runState (mempty :: [ContextToken])
  . runT $ source (tagging blob tree)
      ~> contextualizing blob

type ContextToken = (Text, Maybe Range)

type Contextualizer
  = Eff (StateC [ContextToken]
  ( Eff (ErrorC TranslationError
  ( Eff VoidC))))

contextualizing :: Blob -> Machine.ProcessT Contextualizer Token Symbol
contextualizing Blob{..} = repeatedly $ await >>= \case
  Enter x r -> enterScope (x, r)
  Exit x r  -> exitScope (x, r)
  Identifier iden rng -> lift State.get >>= \case
    ((x, r):("Context", cr):xs) | x `elem` symbolsToSummarize
      -> yield $ Symbol iden x (fmap fst xs) (slice r) (slice cr)
    ((x, r):xs) | x `elem` symbolsToSummarize
      -> yield $ Symbol iden x (fmap fst xs) (slice r) (slice rng)
    _ -> pure ()
  where
    slice = fmap (stripEnd . Source.toText . flip Source.slice blobSource)

enterScope, exitScope :: ContextToken -> Machine.PlanT k Symbol Contextualizer ()
enterScope c = lift (State.modify (c :))
exitScope  c = lift State.get >>= \case
  (x:xs) -> when (x == c) (lift (State.modify (const xs)))
  cs     -> lift (Error.throwError (UnbalancedPair c cs))

data TranslationError = UnbalancedPair ContextToken [ContextToken]
  deriving (Eq, Show)
