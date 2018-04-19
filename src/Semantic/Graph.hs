{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Semantic.Graph where

import qualified Analysis.Abstract.ImportGraph as Abstract
import qualified Data.Abstract.Evaluatable as Analysis
import           Data.Abstract.FreeVariables
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import qualified Control.Exception as Exc
import           Data.Abstract.Module
import           Data.Blob
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
import           Data.List (intercalate)
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


-- | Parse a list of files into a 'Package'.
parsePackage :: Members '[Distribute WrappedTask, Files, Task] effs => PackageName -> Parser term -> FilePath -> [FilePath] -> Eff effs (Package term)
parsePackage name parser rootDir paths = Package (PackageInfo name Nothing) . Package.fromModules <$> parseModules parser rootDir paths

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
             => Maybe (Module term) -> Package term -> Eff effs Abstract.ImportGraph
graphImports prelude package = analyze (Analysis.SomeAnalysis (withPrelude prelude (Analysis.evaluatePackage package `asAnalysisForTypeOfPackage` package))) >>= extractGraph
  where
    asAnalysisForTypeOfPackage :: ImportGraphAnalysis term effs value
                               -> Package term
                               -> ImportGraphAnalysis term effs value
    asAnalysisForTypeOfPackage = const

    extractGraph result = case result of
      (Right (Right (Right (Right (Right (Right (Right ((((_, graph), _), _), _))))))), _) -> pure $! graph
      _ -> throwError (toException (Exc.ErrorCall ("graphImports: import graph rendering failed " <> show result)))

    withPrelude Nothing a = a
    withPrelude (Just prelude) a = do
      preludeEnv <- Analysis.evaluateModule prelude *> Analysis.getEnv
      Analysis.withDefaultEnvironment preludeEnv a
