{-# LANGUAGE GADTs, DeriveAnyClass, LambdaCase, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Tagging
( runTagging
, Tag(..)
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Control.Effect as Eff
import           Control.Effect.Error as Error
import qualified Control.Effect.State as State
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Blob
import           Data.Location
import           Data.Machine as Machine
import qualified Data.Source as Source
import           Data.Term
import           Data.Text hiding (empty)
import           Tags.Taggable

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module"]

data Tag
  = Tag
  { name :: Text
  , kind :: Text
  , span :: Span
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON)

runTagging :: (IsTaggable syntax)
  => Blob
  -> Term syntax Location
  -> Either TranslationError [Tag]
runTagging blob tree
  = Eff.run
  . Error.runError
  . State.evalState mempty
  . runT $ source (tagging blob tree)
      ~> contextualizing blob

type ContextToken = (Text, Maybe Range)

type Contextualizer
  = Eff (StateC [ContextToken]
  ( Eff (ErrorC TranslationError
  ( Eff VoidC))))

contextualizing :: Blob -> Machine.ProcessT Contextualizer Token Tag
contextualizing Blob{..} = repeatedly $ await >>= \case
  Enter x r -> enterScope (x, r)
  Exit  x r -> exitScope (x, r)
  Iden iden span docsLiteralRange -> lift State.get >>= \case
    ((x, r):("Context", cr):xs) | x `elem` symbolsToSummarize
      -> yield $ Tag iden x span (fmap fst xs) (slice r) (slice cr)
    ((x, r):xs) | x `elem` symbolsToSummarize
      -> yield $ Tag iden x span (fmap fst xs) (slice r) (slice docsLiteralRange)
    _ -> pure ()
  where
    slice = fmap (stripEnd . Source.toText . flip Source.slice blobSource)

enterScope, exitScope :: ContextToken -> Machine.PlanT k Tag Contextualizer ()
enterScope c = lift (State.modify (c :))
exitScope  c = lift State.get >>= \case
  (x:xs) -> when (x == c) (lift (State.modify (const xs)))
  cs     -> lift (State.modify (const cs)) -- Just continue on if it's unbalanced

data TranslationError = UnbalancedPair ContextToken [ContextToken]
  deriving (Eq, Show)
