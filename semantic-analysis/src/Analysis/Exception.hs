module Analysis.Exception
( Exception(..)
, ExcSet(..)
) where

import           Analysis.Name
import qualified Data.Set as Set

newtype Exception = Exception { exceptionName :: String }
  deriving (Eq, Ord, Show)

-- | Sets whose elements are each a variable or an exception.
newtype ExcSet = ExcSet { values :: Set.Set (Either Name Exception) }
