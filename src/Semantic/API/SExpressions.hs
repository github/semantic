{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.SExpressions (parseSExpressionBuilder) where

import Control.Effect
import Control.Exception
import Data.Blob
import Data.ByteString.Builder
import Parsing.Parser
import Semantic.API.Parse
import Semantic.Task as Task
import Serializing.Format

parseSExpressionBuilder :: (Traversable t, Member Distribute sig, ParseEffects sig m)
  => t Blob -> m Builder
parseSExpressionBuilder = distributeFoldMap go
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m) => Blob -> m Builder
    go blob@Blob{..} = doParse blobLanguage blob (\_ (SomeTerm t) -> runSerialize Plain (SExpression ByConstructorName) t)
