module Main where

import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Map
import Data.Set
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  let (a, b) = files args in do
    a' <- parseModuleFile a
    b' <- parseModuleFile b
    return (a', b')
  return ()

parseModuleFile :: FilePath -> IO (ParseResult HsModule)
parseModuleFile file = do
  contents <- readFile file
  return $ parseModule contents

moduleToTerm :: HsModule -> Term a Info
moduleToTerm (HsModule loc name exports imports declarations) = info :< Indexed terms where
  info = Info Range { start = 0, end = 0 } Data.Set.empty
  exportTerms = exportSpecToTerm <$> maybe [] id exports
  importTerms = importDeclarationToTerm <$> imports
  declarationTerms = declarationToTerm <$> declarations
  terms = exportTerms ++ importTerms ++ declarationTerms

exportSpecToTerm :: HsExportSpec -> Term a Info
exportSpecToTerm spec = _

importDeclarationToTerm :: HsImportDecl -> Term a Info
importDeclarationToTerm declaration = _

declarationToTerm :: HsDecl -> Term a Info
declarationToTerm declaration = _

files (a : as) = (a, file as) where
  file (a : as) = a
