{-# LANGUAGE DeriveAnyClass #-}
module Language.TypeScript.Resolution
  ( ImportPath (..)
  , IsRelative (..)
  , importPath
  , toName
  , resolveWithNodejsStrategy
  , resolveModule
  , resolveNonRelativePath
  , javascriptExtensions
  -- , evalRequire
  , typescriptExtensions
  ) where

import Prologue

import           Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import           Proto3.Suite
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import           System.FilePath.Posix

import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import Control.Abstract.ScopeGraph (Allocator)
import qualified Data.Abstract.Module as M
import           Data.Abstract.Package
import           Data.Abstract.Path
import qualified Data.Language as Language

data IsRelative = Unknown | Relative | NonRelative
  deriving (Bounded, Enum, Finite, MessageField, Named, Eq, Generic, Hashable, Ord, Show, ToJSON, NFData)

instance Primitive IsRelative where
  encodePrimitive = Encode.enum
  decodePrimitive = either (const def) id <$> Decode.enum
  primType _ = Named (Single (nameOf (Proxy @IsRelative)))

instance HasDefault IsRelative where
  def = Unknown

data ImportPath = ImportPath { unPath :: FilePath, pathIsRelative :: IsRelative }
  deriving (Eq, Generic, Hashable, Message, Named, Ord, Show, ToJSON, NFData)

instance MessageField ImportPath where
  encodeMessageField num = Encode.embedded num . encodeMessage (fieldNumber 1)
  decodeMessageField = fromMaybe def <$> Decode.embedded (decodeMessage (fieldNumber 1))
  protoType _ = messageField (Prim $ Named (Single (nameOf (Proxy @ImportPath)))) Nothing

instance HasDefault ImportPath where
  def = ImportPath mempty Relative

-- TODO: fix the duplication present in this and Python
importPath :: Text -> ImportPath
importPath str = let path = stripQuotes str in ImportPath (T.unpack path) (pathType path)
  where
    pathType xs | not (T.null xs), T.head xs == '.' = Relative -- TODO: fix partiality
                | otherwise = NonRelative

toName :: ImportPath -> Name
toName = name . T.pack . unPath

-- Node.js resolution algorithm: https://nodejs.org/api/modules.html#modules_all_together
--
-- NB: TypeScript has a couple of different strategies, but the main one (and the
-- only one we support) mimics Node.js.
resolveWithNodejsStrategy :: ( Member (Modules address value) sig
                             , Member (Reader M.ModuleInfo) sig
                             , Member (Reader PackageInfo) sig
                             , Member (Reader Span) sig
                             , Member (Resumable (BaseError ResolutionError)) sig
                             , Member Trace sig
                             , Carrier sig m
                             )
                          => ImportPath
                          -> [String]
                          -> Evaluator term address value m M.ModulePath
resolveWithNodejsStrategy (ImportPath path NonRelative) exts = resolveNonRelativePath path exts
resolveWithNodejsStrategy (ImportPath path _)    exts        = resolveRelativePath path exts

-- | Resolve a relative TypeScript import to a known 'ModuleName' or fail.
--
-- import { b } from "./moduleB" in /root/src/moduleA.ts
--
-- /root/src/moduleB.ts
-- /root/src/moduleB/package.json (if it specifies a "types" property)
-- /root/src/moduleB/index.ts
resolveRelativePath :: ( Member (Modules address value) sig
                       , Member (Reader M.ModuleInfo) sig
                       , Member (Reader PackageInfo) sig
                       , Member (Reader Span) sig
                       , Member (Resumable (BaseError ResolutionError)) sig
                       , Member Trace sig
                       , Carrier sig m
                       )
                    => FilePath
                    -> [String]
                    -> Evaluator term address value m M.ModulePath
resolveRelativePath relImportPath exts = do
  M.ModuleInfo{..} <- currentModule
  let relRootDir = takeDirectory modulePath
  let path = joinPaths relRootDir relImportPath
  trace ("attempting to resolve (relative) require/import " <> show relImportPath)
  resolveModule path exts >>= either notFound (\x -> x <$ traceResolve relImportPath path)
  where
    notFound xs = throwResolutionError $ NotFoundError relImportPath xs Language.TypeScript

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
resolveNonRelativePath :: ( Member (Modules address value) sig
                          , Member (Reader M.ModuleInfo) sig
                          , Member (Reader PackageInfo) sig
                          , Member (Reader Span) sig
                          , Member (Resumable (BaseError ResolutionError)) sig
                          , Member Trace sig
                          , Carrier sig m
                          )
                       => FilePath
                       -> [String]
                       -> Evaluator term address value m M.ModulePath
resolveNonRelativePath name exts = do
  M.ModuleInfo{..} <- currentModule
  go "." modulePath mempty
  where
    nodeModulesPath dir = takeDirectory dir </> "node_modules" </> name
    -- Recursively search in a 'node_modules' directory, stepping up a directory each time.
    go root path searched = do
      trace ("attempting to resolve (non-relative) require/import " <> show name)
      res <- resolveModule (nodeModulesPath path) exts
      case res of
        Left xs | parentDir <- takeDirectory path , root /= parentDir -> go root parentDir (searched <> xs)
                | otherwise -> notFound (searched <> xs)
        Right m -> m <$ traceResolve name m
    notFound xs = throwResolutionError $ NotFoundError name xs Language.TypeScript

-- | Resolve a module name to a ModulePath.
resolveModule :: ( Member (Modules address value) sig
                 , Member (Reader PackageInfo) sig
                 , Member Trace sig
                 , Carrier sig m
                 )
              => FilePath -- ^ Module path used as directory to search in
              -> [String] -- ^ File extensions to look for
              -> Evaluator term address value m (Either [FilePath] M.ModulePath)
resolveModule path' exts = do
  let path = makeRelative "." path'
  PackageInfo{..} <- currentPackage
  let packageDotJSON = Map.lookup (path </> "package.json") packageResolutions
  let searchPaths =  ((path <.>) <$> exts)
                  <> maybe mempty (:[]) packageDotJSON
                  <> (((path </> "index") <.>) <$> exts)
  trace ("searching in " <> show searchPaths)
  maybe (Left searchPaths) Right <$> resolve searchPaths

typescriptExtensions :: [String]
typescriptExtensions = ["ts", "tsx", "d.ts"]

javascriptExtensions :: [String]
javascriptExtensions = ["js"]

-- evalRequire :: ( AbstractValue term address value m
--                , Member (Allocator address) sig
--                , Member (Deref value) sig
--                , Member (Env address) sig
--                , Member (Modules address value) sig
--                , Member (State (Heap address address value)) sig
--                , Ord address
--                , Carrier sig m
--                )
--             => M.ModulePath
--             -> Name
--             -> Evaluator term address value m value
-- evalRequire modulePath alias = letrec' alias $ \addr ->
--   unit <$ makeNamespace alias addr Nothing (bindAll . fst . snd =<< require modulePath)
