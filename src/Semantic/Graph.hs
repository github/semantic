{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Semantic.Graph where

import qualified Analysis.Abstract.ImportGraph as Abstract
import qualified Data.Abstract.Evaluatable as Analysis
import           Data.Abstract.FreeVariables
import           Data.Abstract.Module
import           Data.AST
import           Data.Blob
import           Data.ByteString.Char8 as BC (pack)
import           Data.Output
import           Data.Record
import           Data.Term
import qualified GHC.TypeLits as TypeLevel
import           Language.Preluded
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Renderer
import           Semantic.IO (Files, NoLanguageForBlob (..))
import           Semantic.Task
import           System.FilePath.Posix

graph :: (Members '[Distribute WrappedTask, Files, Task, Exc SomeException] effs) => GraphRenderer output -> Blob -> Eff effs ByteString
graph renderer Blob{..}
  | Just (SomeAnalysisParser parser exts preludePath) <- someAnalysisParser
    (Proxy :: Proxy '[ Analysis.Evaluatable, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) <$> blobLanguage = do
    let rootDir = takeDirectory blobPath
    paths <- filter (/= blobPath) <$> listFiles rootDir exts
    prelude <- traverse (parseModule parser Nothing) preludePath
    package <- parsePackage (packageName blobPath) parser rootDir (blobPath : paths)
    graphImports prelude package >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer -> pure . Abstract.renderImportGraph

  | otherwise = throwError (SomeException (NoLanguageForBlob blobPath))

  where packageName = name . BC.pack . dropExtensions . takeFileName
