{-# LANGUAGE GADTs, RankNTypes #-}
module Semantic.Parse
  ( runParse
  , runPythonParse
  , runRubyParse
  , runTypeScriptParse
  , runJSONParse
  ) where

import Analysis.ConstructorName (ConstructorName)
import Analysis.Declaration (HasDeclaration, declarationAlgebra)
import Analysis.PackageDef (HasPackageDef)
import Control.Monad.Effect.Exception
import Data.AST
import Data.Blob
import Data.JSON.Fields
import Data.Quieterm
import Data.Record
import Data.Term
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Graph
import Rendering.Renderer
import Semantic.IO (noLanguageForBlob)
import Semantic.Task
import Serializing.Format
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript
import qualified Language.JSON.Assignment as JSON
import qualified Language.Python.Assignment as Python

runParse :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs) => TermRenderer output -> [Blob] -> Eff effs Builder
runParse JSONTermRenderer             = withParsedBlobs renderJSONError (render . renderJSONTerm) >=> serialize JSON
runParse SExpressionTermRenderer      = withParsedBlobs (\_ _ -> mempty) (const (serialize (SExpression ByConstructorName)))
runParse ShowTermRenderer             = withParsedBlobs (\_ _ -> mempty) (const (serialize Show . quieterm))
runParse (SymbolsTermRenderer fields) = withParsedBlobs (\_ _ -> mempty) (\ blob -> decorate (declarationAlgebra blob) >=> render (renderSymbolTerms . renderToSymbols fields blob)) >=> serialize JSON
runParse DOTTermRenderer              = withParsedBlobs (\_ _ -> mempty) (const (render renderTreeGraph)) >=> serialize (DOT (termStyle "terms"))

-- NB: Our gRPC interface requires concrete 'Term's for each language to know
-- how to encode messages, so we have dedicated functions for parsing each
-- supported language.
runRubyParse :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs)
  => [Blob] -> Eff effs [Either SomeException (Term (Sum Ruby.Syntax) ())]
runRubyParse = flip distributeFor $ \blob ->
    (Right . (() <$) <$> parse rubyParser blob) `catchError` (pure . Left)

runTypeScriptParse :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs)
  => [Blob] -> Eff effs [Either SomeException (Term (Sum TypeScript.Syntax) ())]
runTypeScriptParse = flip distributeFor $ \blob -> do
    (Right . (() <$) <$> parse typescriptParser blob) `catchError` (pure . Left)

runPythonParse :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs)
  => [Blob] -> Eff effs [Either SomeException (Term (Sum Python.Syntax) ())]
runPythonParse = flip distributeFor $ \blob -> do
    (Right . (() <$) <$> parse pythonParser blob) `catchError` (pure . Left)

runJSONParse :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs)
  => [Blob] -> Eff effs [Either SomeException (Term (Sum JSON.Syntax) ())]
runJSONParse = flip distributeFor $ \blob -> do
    (Right . (() <$) <$> parse jsonParser blob) `catchError` (pure . Left)

withParsedBlobs ::
  ( Member Distribute effs
  , Member (Exc SomeException) effs
  , Member Task effs
  , Monoid output
  )
  => (Blob -> String -> output)
  -> ( forall syntax .
        ( ConstructorName syntax
        , Foldable syntax
        , Functor syntax
        , HasDeclaration syntax
        , HasPackageDef syntax
        , Show1 syntax
        , ToJSONFields1 syntax
        ) => Blob -> Term syntax (Record Location) -> Eff effs output
      )
  -> [Blob]
  -> Eff effs output
withParsedBlobs onError render = distributeFoldMap $ \blob ->
  (parseSomeBlob blob >>= withSomeTerm (render blob)) `catchError` \(SomeException e) ->
    pure (onError blob (show e))

parseSomeBlob :: (Member (Exc SomeException) effs, Member Task effs) => Blob -> Eff effs (SomeTerm '[ConstructorName, Foldable, Functor, HasDeclaration, HasPackageDef, Show1, ToJSONFields1] (Record Location))
parseSomeBlob blob@Blob{..} = maybe (noLanguageForBlob blobPath) (`parse` blob) (someParser blobLanguage)
