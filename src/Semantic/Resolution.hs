{-# LANGUAGE ConstraintKinds, DeriveAnyClass, DerivingStrategies, GADTs, GeneralizedNewtypeDeriving, KindSignatures,
             ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
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
import           Source.Source
import           System.FilePath.Posix


nodeJSResolutionMap :: (Member Files sig, Carrier sig m, MonadIO m) => FilePath -> Text -> [FilePath] -> m (Map FilePath FilePath)
nodeJSResolutionMap rootDir prop excludeDirs = do
  files <- findFiles rootDir [".json"] excludeDirs
  let packageFiles = fileForPath <$> filter ((==) "package.json" . takeFileName) files
  blobs <- readBlobs (FilesFromPaths packageFiles)
  pure $ fold (mapMaybe (lookup prop) blobs)
  where
    lookup :: Text -> Blob -> Maybe (Map FilePath FilePath)
    lookup k b@Blob{..} = decodeStrict (sourceBytes blobSource) >>= lookupProp (blobPath b) k

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
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

runResolution :: ResolutionC m a -> m a
runResolution = runResolutionC

newtype ResolutionC m a = ResolutionC { runResolutionC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Member Files sig, Carrier sig m, MonadIO m) => Carrier (Resolution :+: sig) (ResolutionC m) where
  eff (R other) = ResolutionC . eff . handleCoercible $ other
  eff (L op) = case op of
    NodeJSResolution dir prop excludeDirs k -> nodeJSResolutionMap dir prop excludeDirs >>= k
    NoResolution                          k -> k Map.empty
