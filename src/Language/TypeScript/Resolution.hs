{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Language.TypeScript.Resolution
  ( ImportPath (..)
  , IsRelative (..)
  , importPath
  , toName
  , resolveWithNodejsStrategy
  , resolveModule
  , resolveNonRelativePath
  , javascriptExtensions
  , typescriptExtensions
  ) where

import qualified Data.Map as Map
import           System.FilePath.Posix

import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.Package
import           Data.Abstract.Path
import           Data.ImportPath
import qualified Data.Language as Language
import qualified System.Path   as Path

-- Node.js resolution algorithm: https://nodejs.org/api/modules.html#modules_all_together
--
-- NB: TypeScript has a couple of different strategies, but the main one (and the
-- only one we support) mimics Node.js.
resolveWithNodejsStrategy :: ( Has (Modules address value) sig m
                             , Has (Reader M.ModuleInfo) sig m
                             , Has (Reader PackageInfo) sig m
                             , Has (Reader Span) sig m
                             , Has (Resumable (BaseError ResolutionError)) sig m
                             , Has Trace sig m
                             )
                          => ImportPath
                          -> [String]
                          -> Evaluator term address value m M.ModulePath
resolveWithNodejsStrategy (ImportPath path NonRelative) exts = resolveNonRelativePath (Path.relDir path) exts
resolveWithNodejsStrategy (ImportPath path _)    exts        = resolveRelativePath (Path.rel path) exts

-- | Resolve a relative TypeScript import to a known 'ModuleName' or fail.
--
-- import { b } from "./moduleB" in /root/src/moduleA.ts
--
-- /root/src/moduleB.ts
-- /root/src/moduleB/package.json (if it specifies a "types" property)
-- /root/src/moduleB/index.ts
resolveRelativePath :: ( Has (Modules address value) sig m
                       , Has (Reader M.ModuleInfo) sig m
                       , Has (Reader PackageInfo) sig m
                       , Has (Reader Span) sig m
                       , Has (Resumable (BaseError ResolutionError)) sig m
                       , Has Trace sig m
                       )
                    => Path.RelFileDir
                    -> [String]
                    -> Evaluator term address value m M.ModulePath
resolveRelativePath relImportPath exts = do
  M.ModuleInfo{..} <- currentModule
  let relRootDir = Path.takeDirectory modulePath
  let path = relRootDir `joinPaths` relImportPath
  trace ("attempting to resolve (relative) require/import " <> show relImportPath)
  resolveModule path exts >>= either notFound (\x -> x <$ traceResolve relImportPath path)
  where
    notFound xs = throwResolutionError $ NotFoundError (Path.toAbsRel relImportPath) xs Language.TypeScript

-- | Resolve a non-relative TypeScript import to a known 'ModuleName' or fail.
--
-- import { b } from "moduleB" in source file /root/src/moduleA.ts
--
-- /root/src/node_modules/moduleB.ts
-- /root/src/node_modules/moduleB/package.json (if it specifies a "types" property)
-- /root/src/node_modules/moduleB/index.ts
--
-- /root/node_modules/moduleB.ts, etc
-- /node_modules/moduleB.ts, etc
resolveNonRelativePath :: ( Has (Modules address value) sig m
                          , Has (Reader M.ModuleInfo) sig m
                          , Has (Reader PackageInfo) sig m
                          , Has (Reader Span) sig m
                          , Has (Resumable (BaseError ResolutionError)) sig m
                          , Has Trace sig m
                          )
                       => Path.RelDir
                       -> [String]
                       -> Evaluator term address value m M.ModulePath
resolveNonRelativePath name exts = do
  M.ModuleInfo{..} <- currentModule
  go (Path.toAbsRel Path.currentDir) (Path.takeDirectory modulePath) mempty
  where
    nodeModulesPath dir = dir Path.</> Path.relDir "node_modules" Path.</> name
    -- Recursively search in a 'node_modules' directory, stepping up a directory each time.
    go root path searched = do
      trace ("attempting to resolve (non-relative) require/import " <> show name)
      res <- resolveModule (Path.toFileDir $ nodeModulesPath path) exts
      case res of
        Left xs | Just parentDir <- Path.takeSuperDirectory path , root /= path -> go root parentDir (searched <> xs)
                | otherwise -> notFound (searched <> xs)
        Right m -> m <$ traceResolve name m
    notFound xs = throwResolutionError $ NotFoundError (Path.toAbsRel $ Path.toFileDir name) xs Language.TypeScript

-- | Resolve a module name to a ModulePath.
resolveModule :: ( Has (Modules address value) sig m
                 , Has (Reader PackageInfo) sig m
                 , Has Trace sig m
                 )
              => Path.AbsRelFileDir -- ^ Module path used as directory to search in
              -> [String] -- ^ File extensions to look for
              -> Evaluator term address value m (Either [M.ModulePath] M.ModulePath)
resolveModule path' exts = do
  let path = makeRelative "." $ Path.toString path'
  PackageInfo{..} <- currentPackage
  let packageDotJSON = Map.lookup (path </> "package.json") packageResolutions
  let searchPaths' =  ((path <.>) <$> exts)
                  <> maybe mempty (:[]) packageDotJSON
                  <> (((path </> "index") <.>) <$> exts)
  let searchPaths = fmap Path.absRel searchPaths'
  trace ("searching in " <> show searchPaths)
  maybe (Left searchPaths) Right <$> resolve searchPaths

typescriptExtensions :: [String]
typescriptExtensions = ["ts", "tsx", "d.ts"]

javascriptExtensions :: [String]
javascriptExtensions = ["js"]
