module Analysis.Exception
( Exception(..)
, ExcSet(..)
) where

import           Analysis.Name
import qualified Data.Set as Set

newtype Exception = Exception { exceptionName :: String }
  deriving (Eq, Ord, Show)

newtype ExcSet = ExcSet { values :: Set.Set (Either Name Exception) }
