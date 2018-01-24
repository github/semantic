{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Imports
( renderModuleTerms
, renderToImports
) where

import Analysis.Declaration
import Data.Aeson
import Data.Blob
import Data.Maybe (mapMaybe)
import Data.Record
import Data.Span
import Data.Term
import GHC.Generics
import System.FilePath.Posix (takeBaseName)
import qualified Data.Text as T
import qualified Data.Map as Map
import Rendering.TOC (termTableOfContentsBy, declaration, getDeclaration, toCategoryName)


-- | Render terms to final JSON structure.
renderModuleTerms :: [Value] -> Map.Map T.Text Value
renderModuleTerms = Map.singleton "modules" . toJSON

-- | Render a 'Term' to a list of symbols (See 'Symbol').
renderToImports :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> [Value]
renderToImports blob term = toJSON <$> (termToModules blob term)
  where
    termToModules :: (HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> [Module]
    termToModules blob@Blob{..} term = case mapMaybe (moduleSummary blob declarations) declarations of
        [] -> [makeModule defaultModuleName blob declarations]
        modules -> modules
      where
        declarations = termTableOfContentsBy declaration term
        defaultModuleName = T.pack (takeBaseName blobPath)

makeModule :: (HasField fields Span, HasField fields (Maybe Declaration)) => T.Text -> Blob -> [Record fields] -> Module
makeModule name Blob{..} ds = Module name (T.pack blobPath) (T.pack . show <$> blobLanguage) (mapMaybe importSummary ds) (mapMaybe declarationSummary ds) (mapMaybe referenceSummary ds)

moduleSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Blob -> [Record fields] -> Record fields -> Maybe Module
moduleSummary blob declarations record = case getDeclaration record of
  Just ModuleDeclaration{..} -> Just $ makeModule declarationIdentifier blob declarations
  _ -> Nothing

declarationSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Record fields -> Maybe SymbolDeclaration
declarationSummary record = case getDeclaration record of
  Just declaration | FunctionDeclaration{} <- declaration -> Just (makeSymbolDeclaration declaration)
                   | MethodDeclaration{} <- declaration -> Just (makeSymbolDeclaration declaration)
  _ -> Nothing
  where makeSymbolDeclaration declaration = SymbolDeclaration
          { declarationName = declarationIdentifier declaration
          , declarationKind = toCategoryName declaration
          , declarationSpan = getField record
          }

importSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Record fields -> Maybe SymbolImport
importSummary record = case getDeclaration record of
  Just ImportDeclaration{..} -> Just $ SymbolImport declarationIdentifier (getField record)
  _ -> Nothing

referenceSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Record fields -> Maybe SymbolReference
referenceSummary record = case getDeclaration record of
  Just decl@CallReference{..} -> Just  $ SymbolReference declarationIdentifier (toCategoryName decl) (getField record)
  _ -> Nothing

data Module = Module
  { moduleName :: T.Text
  , modulePath :: T.Text
  , moduleLanguage :: Maybe T.Text
  , moduleImports :: [SymbolImport]
  , moduleDeclarations :: [SymbolDeclaration]
  , moduleReferences :: [SymbolReference]
  } deriving (Generic, Eq, Show)

instance ToJSON Module where
  toJSON Module{..} = object [ "name" .= moduleName
                             , "path" .= modulePath
                             , "langauge" .= moduleLanguage
                             , "imports" .= moduleImports
                             , "declarations" .= moduleDeclarations
                             , "references" .= moduleReferences
                             ]

data SymbolDeclaration = SymbolDeclaration
  { declarationName :: T.Text
  , declarationKind :: T.Text
  , declarationSpan :: Span
  } deriving (Generic, Eq, Show)

instance ToJSON SymbolDeclaration where
  toJSON SymbolDeclaration{..} = object [ "name" .= declarationName
                                        , "kind" .= declarationKind
                                        , "span" .= declarationSpan
                                        ]

data SymbolImport = SymbolImport
  { importName :: T.Text
  , importSpan :: Span
  } deriving (Generic, Eq, Show)

instance ToJSON SymbolImport where
  toJSON SymbolImport{..} = object [ "name" .= importName
                                   , "span" .= importSpan ]

data SymbolReference = SymbolReference
  { referenceName :: T.Text
  , referenceKind :: T.Text
  , referenceSpan :: Span
  } deriving (Generic, Eq, Show)

instance ToJSON SymbolReference where
  toJSON SymbolReference{..} = object [ "name" .= referenceName
                                      , "kinds" .= referenceKind
                                      , "span" .= referenceSpan ]
