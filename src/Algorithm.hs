module Algorithm where

import Control.Monad.Free
import Operation

type Algorithm a = Free (Operation a)
