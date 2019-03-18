{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
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
import           Data.Blob
import           Data.Location
import           Data.Machine as Machine
import qualified Data.Source as Source
import           Data.Tag
import           Data.Term
import           Data.Text hiding (empty)
import           Tags.Taggable

runTagging :: (IsTaggable syntax)
  => Blob
  -> [Text]
  -> Term syntax Location
  -> Either TranslationError [Tag]
runTagging blob symbolsToSummarize tree
  = Eff.run
  . Error.runError
  . State.evalState mempty
  . runT $ source (tagging blob tree)
      ~> contextualizing blob symbolsToSummarize

type ContextToken = (Text, Maybe Range)

type Contextualizer
  = StateC [ContextToken]
  ( ErrorC TranslationError PureC)

contextualizing :: Blob -> [Text] -> Machine.ProcessT Contextualizer Token Tag
contextualizing Blob{..} symbolsToSummarize = repeatedly $ await >>= \case
  Enter x r -> enterScope (x, r)
  Exit  x r -> exitScope (x, r)
  Iden iden span docsLiteralRange -> lift State.get >>= \case
    ((x, r):("Context", cr):xs) | x `elem` symbolsToSummarize
      -> yield $ Tag iden x span (fmap fst xs) (firstLine (slice r)) (slice cr)
    ((x, r):xs) | x `elem` symbolsToSummarize
      -> yield $ Tag iden x span (fmap fst xs) (firstLine (slice r)) (slice docsLiteralRange)
    _ -> pure ()
  where
    slice = fmap (stripEnd . Source.toText . flip Source.slice blobSource)
    firstLine = fmap (fst . breakOn "\n")

enterScope, exitScope :: ContextToken -> Machine.PlanT k Tag Contextualizer ()
enterScope c = lift (State.modify (c :))
exitScope  c = lift State.get >>= \case
  (x:xs) -> when (x == c) (lift (State.modify (const xs)))
  cs     -> lift (State.modify (const cs)) -- Just continue on if it's unbalanced

data TranslationError = UnbalancedPair ContextToken [ContextToken]
  deriving (Eq, Show)
