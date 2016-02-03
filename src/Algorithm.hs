module Algorithm where

import Control.Monad.Free
import Operation

-- | A lazily-produced AST for diffing.
type Algorithm a annotation = Free (Operation a annotation)
