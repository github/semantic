module Analysis.Module
( Module(..)
, ModuleSet(..)
, link
) where

import           Analysis.Name
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

data Module a = Module
  { body    :: Map.Map Name a -> a
  , imports :: Set.Set Name
  , exports :: Map.Map Name a
  , unknown :: Set.Set Name
  }

newtype ModuleSet a = ModuleSet { getModuleSet :: Map.Map Name (Module a) }

instance Semigroup (ModuleSet a) where
  m1 <> m2 = ModuleSet ((link m2 <$> getModuleSet m1) <> (link m1 <$> getModuleSet m2))

link :: ModuleSet a -> Module a -> Module a
link (ModuleSet ms) m = Module body' (imports m Set.\\ Map.keysSet ms) (exports m) unknown' where
  (unknown', body') = foldl' (uncurry resolveSymbolsInModule) (unknown m, body m) (Map.restrictKeys ms (imports m))
  resolveSymbolsInModule unknown body m = (unknown Set.\\ Map.keysSet (exports m), body . mappend (Map.restrictKeys (exports m) unknown))
