import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo
import Data.Maybe
import System.Directory
import Distribution.System
import System.FilePath.Posix

main = defaultMainWithHooks simpleUserHooks {
  preConf = makeScannerLib,
  confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs,
  postCopy = copyScannerLib,
  postClean = cleanScannerLib
}

makeScannerLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeScannerLib _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
  rawSystemExit verbosity "env" ["mkdir", "-p", "lib"]
  let flag =  if buildOS == OSX then "-std=c++11" else "-std=c++0x"
  rawSystemExit verbosity "env" ["g++", flag, "-lstdc++", "-Ivendor/tree-sitter-ruby/src/", "-fPIC", "vendor/tree-sitter-ruby/src/scanner.cc", "-c", "-o", "lib/scanner.o"]
  rawSystemExit verbosity "env" ["ar", "rcvs", "lib/libscanner.a", "lib/scanner.o"]
  pure emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libBuild = libBuildInfo lib
  dir <- getCurrentDirectory
  pure localBuildInfo {
    localPkgDescr = packageDescription {
      library = Just $ lib {
        libBuildInfo = libBuild {
          extraLibDirs = (dir </> "lib") : extraLibDirs libBuild
        }
      }
    }
  }

copyScannerLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyScannerLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    rawSystemExit verbosity "cp" ["lib/libscanner.a", libPref]

cleanScannerLib :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanScannerLib _ flags _ _ = do
  let verbosity = fromFlag $ cleanVerbosity flags
  dir <- getCurrentDirectory
  rawSystemExit verbosity "env" ["rm", "-rf", dir </> "lib"]
