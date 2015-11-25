module Algorithm where

import Control.Monad.Free
import Operation

type Algorithm a annotation = Free (Operation a annotation)
