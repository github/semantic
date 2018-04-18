{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Semantic.Graph where

import qualified Analysis.Abstract.ImportGraph as Abstract
import qualified Data.Abstract.Evaluatable as Analysis
import           Data.Abstract.FreeVariables
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Blob
import           Data.List (intercalate)
import           Data.ByteString.Char8 as BC (pack)
import           Data.Output
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Renderer
import           Semantic.IO (Files, NoLanguageForBlob (..))
import           Semantic.Task
import           System.FilePath.Posix

graph :: (Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry] effs)
      => Maybe FilePath
      -> GraphRenderer output
      -> Blob
      -> Eff effs ByteString
graph maybeRootDir renderer Blob{..}
  | Just (SomeAnalysisParser parser exts preludePath) <- someAnalysisParser
    (Proxy :: Proxy '[ Analysis.Evaluatable, Analysis.Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) <$> blobLanguage = do
    let rootDir = fromMaybe (takeDirectory blobPath) maybeRootDir
    paths <- filter (/= blobPath) <$> listFiles rootDir exts
    prelude <- traverse (parseModule parser Nothing) preludePath
    let name = packageName blobPath
    package <- parsePackage name parser rootDir (blobPath : paths)

    let modulePaths = intercalate "," $ ModuleTable.keys (packageModules (packageBody package))
    writeLog Info ("Package " <> show name <> " loaded") [("paths", modulePaths)]

    graphImports prelude package >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer -> pure . Abstract.renderImportGraph

  | otherwise = throwError (SomeException (NoLanguageForBlob blobPath))

  where packageName = name . BC.pack . dropExtensions . takeFileName
