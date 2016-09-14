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
      relativeIncludeDirs = [ "common", "i18n" ] in do
      dir <- getCurrentDirectory
      let icuLibDir = dir ++ "/vendor/icu/lib"
      let icuSourceDir = dir ++ "/vendor/icu/source/"
      icuLibDirExists <- doesDirectoryExist icuLibDir
      icuSourceDirExists <- doesDirectoryExist icuSourceDir
      let extraLibDirs = P.extraLibDirs libraryBuildInfo
      let includeDirs = P.includeDirs libraryBuildInfo
      return localBuildInfo {
        localPkgDescr = packageDescription {
          P.library = Just $ library {
            P.libBuildInfo = libraryBuildInfo {
              P.extraLibDirs = if icuLibDirExists then icuLibDir : extraLibDirs else extraLibDirs,
              P.includeDirs = if icuSourceDirExists then ((icuSourceDir ++) <$> relativeIncludeDirs) ++ includeDirs else includeDirs
            }
          }
        }
      }
