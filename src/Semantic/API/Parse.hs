{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.Parse (doParse, TermConstraints) where

import           Analysis.ConstructorName (ConstructorName)
import           Control.Effect
import           Control.Effect.Error
import           Control.Exception
import           Data.Abstract.Declarations
import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.Blob
import           Data.Language
import           Data.Location
import           Data.Term
import qualified Data.Text as T
import           GHC.Generics
import           Parsing.Parser
import qualified Proto3.Suite as Proto3
import qualified Proto3.Suite.Types as P
import           Rendering.Symbol
import           Semantic.Task as Task
import           Servant.API
import           Tags.Taggable

type TermConstraints =
 '[ Taggable
  , Declarations1
  , ConstructorName
  , HasTextElement
  ]

doParse :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m)
  => Language -> Blob -> (Blob -> SomeTerm TermConstraints Location -> output) -> m output
doParse lang blob render = case lang of
  Go         -> render blob . SomeTerm <$> parse goParser blob
  Haskell    -> render blob . SomeTerm <$> parse haskellParser blob
  Java       -> render blob . SomeTerm <$> parse javaParser blob
  JavaScript -> render blob . SomeTerm <$> parse typescriptParser blob
  JSON       -> render blob . SomeTerm <$> parse jsonParser blob
  JSX        -> render blob . SomeTerm <$> parse typescriptParser blob
  Markdown   -> render blob . SomeTerm <$> parse markdownParser blob
  Python     -> render blob . SomeTerm <$> parse pythonParser blob
  Ruby       -> render blob . SomeTerm <$> parse rubyParser blob
  TypeScript -> render blob . SomeTerm <$> parse typescriptParser blob
  PHP        -> render blob . SomeTerm <$> parse phpParser blob
  _          -> noLanguageForBlob (blobPath blob)
