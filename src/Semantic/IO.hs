{-# LANGUAGE DuplicateRecordFields, GADTs, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Semantic.IO
  ( isDirectory
  , findFilesInDir
  ) where

import Prelude hiding (readFile)
import Prologue

import           Data.Language
import           System.Directory (doesDirectoryExist)
import           System.Directory.Tree (AnchoredDirTree (..))
import qualified System.Directory.Tree as Tree
import           System.FilePath
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

isDirectory :: MonadIO m => FilePath -> m Bool
isDirectory path = liftIO (doesDirectoryExist path)

-- Recursively find files in a directory.
findFilesInDir :: (Path.PartClass.AbsRel ar, MonadIO m) => Path.Dir ar -> [String] -> [Path.Dir ar] -> m [Path.File ar]
findFilesInDir path exts excludeDirs = do
  _:/dir <- liftIO $ Tree.build (Path.toString path)
  pure $ (onlyFiles . Tree.filterDir (withExtensions exts) . Tree.filterDir (notIn excludeDirs)) dir
  where
    -- Build a list of only FilePath's (remove directories and failures)
    onlyFiles (Tree.Dir _ fs)   = concatMap onlyFiles fs
    onlyFiles (Tree.Failed _ _) = []
    onlyFiles (Tree.File _ f)   = [Path.file f]

    -- Predicate for Files with one of the extensions in 'exts'.
    withExtensions exts (Tree.File n _)
      | pathIsMinified n            = False
      | takeExtension n `elem` exts = True
      | otherwise                   = False
    withExtensions _ _              = True

    -- Predicate for contents NOT in a directory
    notIn dirs (Tree.Dir n _)
      | (x:_) <- n, x == '.' = False -- Don't include directories that start with '.'.
      | Path.dir n `elem` dirs = False
      | otherwise = True
    notIn _ _ = True
