{-# LANGUAGE GADTs #-}
module Semantic.Graph where

import qualified Data.Abstract.Evaluatable as Analysis
import qualified Analysis.Abstract.ImportGraph as Abstract
import Data.Abstract.FreeVariables
import Data.Blob
import Data.Output
import Parsing.Parser
import Prologue hiding (MonadError(..))
import Rendering.Renderer
import Semantic.IO (NoLanguageForBlob(..), Files)
import Semantic.Task
import System.FilePath.Posix
import Data.ByteString.Char8 as BC (pack)

graph :: (Members '[Distribute WrappedTask, Files, Task, Exc SomeException] effs) => GraphRenderer output -> Blob -> Eff effs ByteString
graph renderer Blob{..}
  | Just (SomeAnalysisParser parser exts) <- someAnalysisParser
    (Proxy :: Proxy '[ Analysis.Evaluatable, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) <$> blobLanguage = do
    let rootDir = takeDirectory blobPath
    paths <- filter (/= blobPath) <$> listFiles rootDir exts
    package <- parsePackage (packageName blobPath) parser rootDir (blobPath : paths)
    graphImports package >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer -> pure . Abstract.renderImportGraph

  | otherwise = throwError (SomeException (NoLanguageForBlob blobPath))

  where packageName = name . BC.pack . dropExtensions . takeFileName
