{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.Resolution
  ( Resolution (..)
  , nodeJSResolutionMap
  , resolutionMap
  , runResolution
  , ResolutionC(..)
  ) where

import           Control.Effect.Carrier
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.Blob
import           Data.Language
import qualified Data.Map as Map
import           Data.Project
import           GHC.Generics (Generic1)
import           Prologue
import           Semantic.Task.Files
import qualified Source.Source as Source
import           System.FilePath.Posix
import qualified System.Path as Path


nodeJSResolutionMap :: (Member Files sig, Carrier sig m, MonadIO m) => FilePath -> Text -> [FilePath] -> m (Map FilePath FilePath)
nodeJSResolutionMap rootDir prop excludeDirs = do
  files <- findFiles (Path.absRel rootDir) [".json"] (fmap Path.absRel excludeDirs)
  let packageFiles = fileForTypedPath <$> filter ((==) (Path.relFile "package.json") . Path.takeFileName) files
  blobs <- readBlobs (FilesFromPaths packageFiles)
  pure $ fold (mapMaybe (lookup prop) blobs)
  where
    lookup :: Text -> Blob -> Maybe (Map FilePath FilePath)
    lookup k b@Blob{..} = decodeStrict (Source.bytes blobSource) >>= lookupProp (blobPath b) k

    lookupProp :: FilePath -> Text -> Object -> Maybe (Map FilePath FilePath)
    lookupProp path k res = flip parseMaybe res $ \obj -> Map.singleton relPkgDotJSONPath . relEntryPath <$> obj .: k
      where relPkgDotJSONPath = makeRelative rootDir path
            relEntryPath x = takeDirectory relPkgDotJSONPath </> x

resolutionMap :: (Member Resolution sig, Carrier sig m) => Project -> m (Map FilePath FilePath)
resolutionMap Project{..} = case projectLanguage of
  TypeScript -> send (NodeJSResolution projectRootDir "types" projectExcludeDirs pure)
  JavaScript -> send (NodeJSResolution projectRootDir "main"  projectExcludeDirs pure)
  _          -> send (NoResolution pure)

data Resolution (m :: * -> *) k
  = NodeJSResolution FilePath Text [FilePath] (Map FilePath FilePath -> m k)
  | NoResolution                              (Map FilePath FilePath -> m k)
  deriving (Functor, Generic1)

instance HFunctor Resolution
instance Effect   Resolution

runResolution :: ResolutionC m a -> m a
runResolution = runResolutionC

newtype ResolutionC m a = ResolutionC { runResolutionC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Member Files sig, Carrier sig m, MonadIO m) => Carrier (Resolution :+: sig) (ResolutionC m) where
  eff (R other) = ResolutionC . eff . handleCoercible $ other
  eff (L op) = case op of
    NodeJSResolution dir prop excludeDirs k -> nodeJSResolutionMap dir prop excludeDirs >>= k
    NoResolution                          k -> k Map.empty
