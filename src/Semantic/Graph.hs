{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Semantic.Graph where

import qualified Analysis.Abstract.ImportGraph as Abstract
import qualified Data.Abstract.Evaluatable as Analysis
import           Data.Abstract.FreeVariables
import           Data.Abstract.Package as Package
import qualified Control.Exception as Exc
import           Data.Abstract.Module
import           Data.File
import           Data.Term
import qualified Data.Syntax as Syntax
import           Data.Abstract.Value (Value)
import           Data.Abstract.Located
import           Data.Abstract.Address
import           Analysis.Abstract.BadAddresses
import           Analysis.Abstract.BadModuleResolutions
import           Analysis.Abstract.BadValues
import           Analysis.Abstract.BadVariables
import           Analysis.Abstract.Evaluating
import           Analysis.Abstract.Quiet
import           Data.ByteString.Char8 as BC (pack)
import           Data.Output
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Renderer
import           Semantic.IO (Files, NoLanguageForBlob (..))
import qualified Semantic.IO as IO
import           Semantic.Task
import           System.FilePath.Posix

graph :: (Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry] effs)
      => Maybe FilePath
      -> GraphRenderer output
      -> File
      -> Eff effs ByteString
graph root renderer file@File{..}
  | Just (SomeAnalysisParser parser exts preludePath) <- someAnalysisParser
    (Proxy :: Proxy '[ Analysis.Evaluatable, Analysis.Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) <$> fileLanguage = do
    parsePackage parser exts preludePath root file >>= graphImports >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer -> pure . Abstract.renderImportGraph

  | otherwise = throwError (SomeException (NoLanguageForBlob filePath))

-- | Parse a list of files into a 'Package'.
parsePackage :: Members '[Distribute WrappedTask, Files, Task] effs
             => Parser term       -- ^ A parser
             -> [String]          -- ^ List of file extensions
             -> Maybe FilePath    -- ^ Prelude (optional).
             -> Maybe FilePath    -- ^ Root directory of this package. If you pass 'Nothing' it will be the parent directory of the entry point.
             -> File              -- ^ Entry point
             -> Eff effs (Package term)
parsePackage parser exts preludePath root File{..} = do
  paths <- filter (/= filePath) <$> listFiles rootDir exts
  prelude <- traverse (parseModule parser Nothing) preludePath
  Package.fromModules (nameFromRoot rootDir) Nothing prelude <$> parseModules parser rootDir paths
  where
    rootDir = fromMaybe (takeDirectory filePath) root
    nameFromRoot = name . BC.pack . dropExtensions . takeFileName

-- | Parse a list of files into 'Module's.
parseModules :: Members '[Distribute WrappedTask, Files, Task] effs => Parser term -> FilePath -> [FilePath] -> Eff effs [Module term]
parseModules parser rootDir paths = distributeFor paths (WrapTask . parseModule parser (Just rootDir))

-- | Parse a file into a 'Module'.
parseModule :: Members '[Files, Task] effs => Parser term -> Maybe FilePath -> FilePath -> Eff effs (Module term)
parseModule parser rootDir path = do
  blob <- readBlob (path, IO.languageForFilePath path)
  moduleForBlob rootDir blob <$> parse parser blob


type ImportGraphAnalysis term effects value =
  Abstract.ImportGraphing
    (BadAddresses (BadModuleResolutions (BadVariables (BadValues (Quietly (Evaluating (Located Precise term) term (Value (Located Precise term))))))))
    effects
    value

-- | Render the import graph for a given 'Package'.
graphImports :: (
                  Show ann
                , Ord ann
                , Apply Analysis.Declarations1 syntax
                , Apply Analysis.Evaluatable syntax
                , Apply FreeVariables1 syntax
                , Apply Functor syntax
                , Apply Ord1 syntax
                , Apply Eq1 syntax
                , Apply Show1 syntax
                , Member Syntax.Identifier syntax
                , Members '[Exc SomeException, Task] effs
                , term ~ Term (Union syntax) ann
                )
             => Package term -> Eff effs Abstract.ImportGraph
graphImports package = analyze (Analysis.SomeAnalysis (Analysis.evaluatePackage package `asAnalysisForTypeOfPackage` package)) >>= extractGraph
  where
    asAnalysisForTypeOfPackage :: ImportGraphAnalysis term effs value
                               -> Package term
                               -> ImportGraphAnalysis term effs value
    asAnalysisForTypeOfPackage = const

    extractGraph result = case result of
      (Right (Right (Right (Right (Right (Right (Right ((((_, graph), _), _), _))))))), _) -> pure $! graph
      _ -> throwError (toException (Exc.ErrorCall ("graphImports: import graph rendering failed " <> show result)))
