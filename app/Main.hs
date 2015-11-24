module Main where

import Diff
import Patch
import Term
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Map
import Data.Maybe
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

data Leaf =
  HsQName HsQName
  | HsCName HsCName
  | HsImportDecl HsImportDecl
  | HsName HsName

parseModuleFile :: FilePath -> IO (ParseResult HsModule)
parseModuleFile file = do
  contents <- readFile file
  return $ parseModule contents

_info = Info Range { start = 0, end = 0 } Data.Set.empty

moduleToTerm :: HsModule -> Term Leaf Info
moduleToTerm (HsModule loc name exports imports declarations) = _info :< Indexed terms where
  exportTerms = exportSpecToTerm <$> fromMaybe [] exports
  importTerms = importDeclarationToTerm <$> imports
  declarationTerms = declarationToTerm <$> declarations
  terms = exportTerms ++ importTerms ++ declarationTerms

exportSpecToTerm :: HsExportSpec -> Term Leaf Info
exportSpecToTerm (HsEVar name) = qualifiedNameToTerm name
exportSpecToTerm (HsEAbs name) = qualifiedNameToTerm name
exportSpecToTerm (HsEThingAll name) = qualifiedNameToTerm name
exportSpecToTerm (HsEThingWith name components) = _info :< (Fixed $ qualifiedNameToTerm name : (componentNameToTerm <$> components))

qualifiedNameToTerm :: HsQName -> Term Leaf Info
qualifiedNameToTerm name = _info :< Leaf (HsQName name)

componentNameToTerm :: HsCName -> Term Leaf Info
componentNameToTerm name = _info :< Leaf (HsCName name)

importDeclarationToTerm :: HsImportDecl -> Term Leaf Info
importDeclarationToTerm declaration = _info :< Leaf (Main.HsImportDecl declaration)

declarationToTerm :: HsDecl -> Term Leaf Info
declarationToTerm declaration = _

files (a : as) = (a, file as) where
  file (a : as) = a
