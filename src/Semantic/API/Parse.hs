{-# LANGUAGE GADTs, ConstraintKinds, TypeOperators, DerivingStrategies #-}
module Semantic.API.Parse
  ( doParse
  , ParseEffects
  , TermConstraints
  , SomeTerm(..)
  , withSomeTerm
  ) where

import Analysis.ConstructorName (ConstructorName)
import Control.Effect
import Control.Effect.Error
import Data.Abstract.Declarations
import Data.Blob
import Data.JSON.Fields
import Data.Language
import Data.Location
import Parsing.Parser
import Semantic.Task as Task
import Tags.Taggable

type ParseEffects sig m = (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m)

type TermConstraints =
 '[ Taggable
  , Declarations1
  , ConstructorName
  , HasTextElement
  , ToJSONFields1
  ]

doParse :: (ParseEffects sig m) => Blob -> m (SomeTerm TermConstraints Location)
doParse blob@Blob{..} = case blobLanguage of
  Go         -> SomeTerm <$> parse goParser blob
  Haskell    -> SomeTerm <$> parse haskellParser blob
  Java       -> SomeTerm <$> parse javaParser blob
  JavaScript -> SomeTerm <$> parse typescriptParser blob
  JSON       -> SomeTerm <$> parse jsonParser blob
  JSX        -> SomeTerm <$> parse typescriptParser blob
  Markdown   -> SomeTerm <$> parse markdownParser blob
  Python     -> SomeTerm <$> parse pythonParser blob
  Ruby       -> SomeTerm <$> parse rubyParser blob
  TypeScript -> SomeTerm <$> parse typescriptParser blob
  PHP        -> SomeTerm <$> parse phpParser blob
  _          -> noLanguageForBlob blobPath
