{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Semantic.Resolution
  ( Resolution (..)
  , nodeJSResolutionMap
  , resolutionMap
  , runResolution
  , ResolutionC(..)
  ) where

import           Analysis.File as File
import           Analysis.Project
import           Control.Algebra
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.Blob
import           Data.Foldable
import           Data.Language
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe.Exts
import           Data.Text (Text)
import           GHC.Generics (Generic1)
import           Semantic.Task.Files
import qualified Source.Source as Source
import           System.FilePath.Posix
import qualified System.Path as Path


nodeJSResolutionMap :: Has Files sig m => FilePath -> Text -> [FilePath] -> m (Map FilePath FilePath)
nodeJSResolutionMap rootDir prop excludeDirs = do
  files <- findFiles (Path.absRel rootDir) [".json"] (fmap Path.absRel excludeDirs)
  let packageFiles = File.fromPath <$> filter ((==) (Path.relFile "package.json") . Path.takeFileName) files
  blobs <- readBlobs (FilesFromPaths packageFiles)
  pure $ fold (mapMaybe (lookup prop) blobs)
  where
    lookup :: Text -> Blob -> Maybe (Map FilePath FilePath)
    lookup k b@Blob{..} = decodeStrict (Source.bytes blobSource) >>= lookupProp (blobPath b) k

    lookupProp :: FilePath -> Text -> Object -> Maybe (Map FilePath FilePath)
    lookupProp path k res = flip parseMaybe res $ \obj -> Map.singleton relPkgDotJSONPath . relEntryPath <$> obj .: k
      where relPkgDotJSONPath = makeRelative rootDir path
            relEntryPath x = takeDirectory relPkgDotJSONPath </> x

resolutionMap :: Has Resolution sig m => Project -> m (Map FilePath FilePath)
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
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Has Files sig m, MonadIO m) => Algebra (Resolution :+: sig) (ResolutionC m) where
  alg (R other) = ResolutionC . alg . handleCoercible $ other
  alg (L op) = case op of
    NodeJSResolution dir prop excludeDirs k -> nodeJSResolutionMap dir prop excludeDirs >>= k
    NoResolution                          k -> k Map.empty
