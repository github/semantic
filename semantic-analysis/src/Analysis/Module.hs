module Analysis.Module
( Module(..)
, moduleBody
, ModuleSet(..)
, fromList
, link
) where

import           Analysis.File
import           Analysis.Name
import           Analysis.Reference
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           System.FilePath as Path

data Module a = Module
  { body    :: Map.Map Name a -> a
  , imports :: Set.Set Name
  , exports :: Map.Map Name a
  , unknown :: Set.Set Name
  }

instance Show a => Show (Module a) where
  showsPrec p (Module b i e u) = showParen (p > 10) (showString "Module" . sp . braces
    ( field "body"    (b mempty) . comma . sp
    . field "imports" i          . comma . sp
    . field "exports" e          . comma . sp
    . field "unknown" u))
    where
    braces b = showChar '{' . sp . b . sp . showChar '}'
    comma = showChar ','
    field n b = showString n . sp . showChar '=' . sp . showsPrec 0 b
    sp = showChar ' '

moduleBody :: Module a -> a
moduleBody m = body m mempty


newtype ModuleSet a = ModuleSet { getModuleSet :: Map.Map Name (Module a) }
  deriving (Show)

instance Semigroup (ModuleSet a) where
  m1 <> m2 = ModuleSet ((link m2 <$> getModuleSet m1) <> (link m1 <$> getModuleSet m2))

instance Monoid (ModuleSet a) where
  mempty = ModuleSet mempty

fromList :: [File (Module a)] -> ModuleSet a
fromList = ModuleSet . Map.fromList . map (\ (File ref mod) -> (refName ref, mod))
  where
  refName (Reference path _) = name (Text.pack (Path.takeBaseName path))

link :: ModuleSet a -> Module a -> Module a
link (ModuleSet ms) m = Module body' (imports m Set.\\ Map.keysSet ms) (exports m) unknown' where
  (unknown', body') = foldl' (uncurry resolveSymbolsInModule) (unknown m, body m) (Map.restrictKeys ms (imports m))
  resolveSymbolsInModule unknown body m = (unknown Set.\\ Map.keysSet (exports m), body . mappend (Map.restrictKeys (exports m) unknown))
