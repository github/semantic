import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Utils
main = defaultMainWithHooks simpleUserHooks { preConf = makeScannerLib }

makeScannerLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeScannerLib _ flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env" ["mkdir", "-p", "lib"]
    rawSystemExit verbosity "env" ["gcc", "-std=c++11", "-Ivendor/tree-sitter-ruby/src/", "-fPIC", "vendor/tree-sitter-ruby/src/scanner.cc", "-c", "-o", "lib/scanner.o"]
    rawSystemExit verbosity "env" ["ar", "rcvs", "lib/libscanner.a", "lib/scanner.o"]
    pure emptyHookedBuildInfo
