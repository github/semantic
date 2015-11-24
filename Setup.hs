import Data.Maybe
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Directory

main = defaultMainWithHooks simpleUserHooks {
  confHook = semanticDiffConfHook
}

semanticDiffConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  return localBuildInfo {
      localPkgDescr = packageDescription {
          PD.library = Just $ library {
              PD.libBuildInfo = libraryBuildInfo {
                  PD.extraLibDirs = (dir ++ "/prototype/External/tree-sitter/out/Release"):(dir ++ "/prototype/External/tree-sitter-c"):PD.extraLibDirs libraryBuildInfo
              }
          }
      }
  }
