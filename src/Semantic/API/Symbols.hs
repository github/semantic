{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.Symbols (parseToSymbols) where

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
import           Semantic.API.Parse
import           Semantic.Task as Task
import           Servant.API
import           Tags.Taggable

parseToSymbols :: ( Member (Error SomeException) sig
           , Member Distribute sig
           , Member Task sig
           , Carrier sig m
           , Monad m
           , Traversable t
           )
  => Maybe String -> t Blob -> m [File]
parseToSymbols fields = distributeFoldMap go
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m) => Blob -> m [File]
    go blob@Blob{..} = doParse blobLanguage blob render `catchError` (\e@(SomeException _) -> pure (pure emptyFile))
      where emptyFile = File (T.pack blobPath) (T.pack (show blobLanguage)) []

    render :: Blob -> SomeTerm TermConstraints Location -> [File]
    render blob (SomeTerm term) = renderToSymbols symbolFields blob term

    symbolFields :: SymbolFields
    symbolFields = maybe defaultSymbolFields parseSymbolFields fields
