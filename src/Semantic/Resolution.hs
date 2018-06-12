{-# LANGUAGE ConstraintKinds, GADTs, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.Resolution where

import           Control.Monad.Effect
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.Blob
import           Data.Project
import qualified Data.Map as Map
import           Data.Source
import           Data.Language
import           Prologue
import           Semantic.IO
import           System.FilePath.Posix


nodeJSResolutionMap :: Member Files effs => FilePath -> Text -> [FilePath] -> Eff effs (Map FilePath FilePath)
nodeJSResolutionMap rootDir prop excludeDirs = do
  files <- findFiles rootDir [".json"] excludeDirs
  let packageFiles = file <$> filter ((==) "package.json" . takeFileName) files
  blobs <- readBlobs (Right packageFiles)
  pure $ fold (mapMaybe (lookup prop) blobs)
  where
    lookup :: Text -> Blob -> Maybe (Map FilePath FilePath)
    lookup k Blob{..} = decodeStrict (sourceBytes blobSource) >>= lookupProp blobPath k

    lookupProp :: FilePath -> Text -> Object -> Maybe (Map FilePath FilePath)
    lookupProp path k res = flip parseMaybe res $ \obj -> Map.singleton relPkgDotJSONPath . relEntryPath <$> obj .: k
      where relPkgDotJSONPath = makeRelative rootDir path
            relEntryPath x = takeDirectory relPkgDotJSONPath </> x

resolutionMap :: Member Resolution effs => Project -> Eff effs (Map FilePath FilePath)
resolutionMap Project{..} = case projectLanguage of
  TypeScript -> send (NodeJSResolution projectRootDir "types" projectExcludeDirs)
  JavaScript -> send (NodeJSResolution projectRootDir "main" projectExcludeDirs)
  _          -> send NoResolution

data Resolution (m :: * -> *) output where
  NodeJSResolution :: FilePath -> Text -> [FilePath] -> Resolution m (Map FilePath FilePath)
  NoResolution     ::                                   Resolution m (Map FilePath FilePath)

instance Effect Resolution where
  handleState c dist (Request (NodeJSResolution path key paths) k) = Request (NodeJSResolution path key paths) (dist . (<$ c) . k)
  handleState c dist (Request NoResolution k) = Request NoResolution (dist . (<$ c) . k)

runResolution :: (Member Files effs, Effects effs) => Eff (Resolution ': effs) a -> Eff effs a
runResolution = interpret $ \ res -> case res of
  NodeJSResolution dir prop excludeDirs -> nodeJSResolutionMap dir prop excludeDirs
  NoResolution                          -> pure Map.empty
