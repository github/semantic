{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Rendering.Imports
( renderToImports
, ModuleSummary(..)
) where

import Analysis.Declaration
import Analysis.ModuleDef
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Blob
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.Record
import Data.Output
import Data.Span
import Data.Term
import GHC.Generics
import System.FilePath.Posix (takeBaseName)
import qualified Data.Text as T
import qualified Data.Map as Map
import Rendering.TOC (termTableOfContentsBy, declaration, getDeclaration, toCategoryName)


newtype ModuleSummary = ModuleSummary (Map.Map T.Text Module) deriving (Eq, Show)

instance Monoid ModuleSummary where
  mempty = ModuleSummary mempty
  mappend (ModuleSummary m1) (ModuleSummary m2) = ModuleSummary (Map.unionWith (<>) m1 m2)

instance Output ModuleSummary where
  toOutput = toStrict . (<> "\n") . encodePretty' defConfig { confCompare = compare, confIndent = Spaces 2 }

instance ToJSON ModuleSummary where
  toJSON (ModuleSummary m) = object [ "modules" .= m ]

renderToImports :: (HasField fields (Maybe ModuleDef), HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> ModuleSummary
renderToImports blob term = ModuleSummary $ toMap (termToModule blob term)
  where
    toMap m@Module{..} = Map.singleton moduleName m
    termToModule :: (HasField fields (Maybe ModuleDef), HasField fields (Maybe Declaration), HasField fields Span, Foldable f, Functor f) => Blob -> Term f (Record fields) -> Module
    termToModule blob@Blob{..} term = makeModule detectModuleName blob declarations
      where
        declarations = termTableOfContentsBy declaration term
        defaultModuleName = T.pack (takeBaseName blobPath)
        detectModuleName = case termTableOfContentsBy moduleDef term of
          x:_ | Just ModuleDef{..} <- getModuleDef x -> moduleDefIdentifier
          _ -> defaultModuleName

makeModule :: (HasField fields Span, HasField fields (Maybe Declaration)) => T.Text -> Blob -> [Record fields] -> Module
makeModule name Blob{..} ds = Module name [T.pack blobPath] (T.pack . show <$> blobLanguage) (mapMaybe importSummary ds) (mapMaybe declarationSummary ds) (mapMaybe referenceSummary ds)


getModuleDef :: HasField fields (Maybe ModuleDef) => Record fields -> Maybe ModuleDef
getModuleDef = getField

-- | Produce the annotations of nodes representing moduleDefs.
moduleDef :: HasField fields (Maybe ModuleDef) => TermF f (Record fields) a -> Maybe (Record fields)
moduleDef (In annotation _) = annotation <$ getModuleDef annotation

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
  Just ImportDeclaration{..} -> Just $ SymbolImport declarationIdentifier declarationAlias (getField record)
  _ -> Nothing

referenceSummary :: (HasField fields (Maybe Declaration), HasField fields Span) => Record fields -> Maybe SymbolReference
referenceSummary record = case getDeclaration record of
  Just decl@CallReference{..} -> Just  $ SymbolReference declarationIdentifier declarationImportIdentifier (toCategoryName decl) (getField record)
  _ -> Nothing

data Module = Module
  { moduleName :: T.Text
  , modulePaths :: [T.Text]
  , moduleLanguage :: Maybe T.Text
  , moduleImports :: [SymbolImport]
  , moduleDeclarations :: [SymbolDeclaration]
  , moduleReferences :: [SymbolReference]
  } deriving (Generic, Eq, Show)

instance Monoid Module where
  mempty = mempty
  mappend (Module n1 p1 l1 i1 d1 r1) (Module _ p2 _ i2 d2 r2) = Module n1 (p1 <> p2) l1 (i1 <> i2) (d1 <> d2) (r1 <> r2)

instance ToJSON Module where
  toJSON Module{..} = object [ "name" .= moduleName
                             , "paths" .= modulePaths
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
                                        -- , "span" .= declarationSpan
                                        ]

data SymbolImport = SymbolImport
  { importName :: T.Text
  , importAlias :: T.Text
  , importSpan :: Span
  } deriving (Generic, Eq, Show)

instance ToJSON SymbolImport where
  toJSON SymbolImport{..} = object [ "name" .= importName
                                   , "alias" .= importAlias
                                   -- , "span" .= importSpan
                                   ]

data SymbolReference = SymbolReference
  { referenceName :: T.Text
  , referenceImport :: Maybe T.Text
  , referenceKind :: T.Text
  , referenceSpan :: Span
  } deriving (Generic, Eq, Show)

instance ToJSON SymbolReference where
  toJSON SymbolReference{..} = object [ "name" .= referenceName
                                      , "import" .= referenceImport
                                      , "kind" .= referenceKind
                                      -- , "span" .= referenceSpan
                                      ]
