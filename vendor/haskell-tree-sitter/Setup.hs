import Data.Maybe
import qualified Distribution.PackageDescription as P
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Process

main = defaultMainWithHooks simpleUserHooks { confHook = conf }

conf :: (P.GenericPackageDescription, P.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
conf x flags = do
  localBuildInfo <- confHook simpleUserHooks x flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ P.library packageDescription
      libraryBuildInfo = P.libBuildInfo library
      relativeIncludeDirs = [ "include", "src", "externals/utf8proc" ] in do
      dir <- getCurrentDirectory
      putStrLn $ "[debug] configuring haskell-tree-sitter within directory: " ++ dir
      return localBuildInfo {
        localPkgDescr = packageDescription {
          P.library = Just $ library {
            P.libBuildInfo = libraryBuildInfo {
              P.includeDirs = (((dir ++ "/vendor/tree-sitter/") ++) <$> relativeIncludeDirs) ++ P.includeDirs libraryBuildInfo
            }
          }
        }
      }
