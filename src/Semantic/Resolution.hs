{-# LANGUAGE ConstraintKinds, GADTs, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.Resolution where

import           Control.Monad.Effect hiding (run)
import           Control.Monad.Effect.Run
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.Blob
import           Data.File
import qualified Data.Map as Map
import           Data.Source
import           Data.Language
import           Prologue
import           Semantic.IO
import           System.FilePath.Posix


nodeJSResolutionMap :: Member Files effs => FilePath -> Language -> [FilePath] -> Eff effs (Map FilePath FilePath)
nodeJSResolutionMap dir lang excludeDirs = do
  files <- findFiles dir [".json"] excludeDirs -- ["node_modules"]
  let packageFiles = file <$> filter ((==) "package.json" . takeFileName) files
  blobs <- readBlobs (Right packageFiles)
  pure $ fold (mapMaybe (lookup (propertyNameForLanguage lang)) blobs)
  where
    -- Entrypoint property name is different for JavaScript vs. TypeScript module resolution.
    propertyNameForLanguage TypeScript = "types"
    propertyNameForLanguage _ = "main"

    lookup :: Text -> Blob -> Maybe (Map FilePath FilePath)
    lookup k Blob{..} = decodeStrict (sourceBytes blobSource) >>= lookupProp blobPath k

    lookupProp :: FilePath -> Text -> Object -> Maybe (Map FilePath FilePath)
    lookupProp path k res = flip parseMaybe res $ \obj -> Map.singleton path <$> obj .: k

nodeResolution :: Member Resolution effs => FilePath -> Language -> [FilePath] -> Eff effs (Map FilePath FilePath)
nodeResolution dir prop = send . NodeJSResolution dir prop

data Resolution output where
  NodeJSResolution :: FilePath -> Language -> [FilePath] -> Resolution (Map FilePath FilePath)

runResolution :: Members '[Files] effs => Eff (Resolution ': effs) a -> Eff effs a
runResolution = interpret $ \ res -> case res of
  NodeJSResolution dir prop excludeDirs -> nodeJSResolutionMap dir prop excludeDirs

instance (Members '[Files] effects, Run effects result rest) => Run (Resolution ': effects) result rest where
  run = run . runResolution
