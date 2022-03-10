module Analysis.Carrier.Statement.State
( -- * Messages
  Message(..)
  -- * Statement carrier
, runStatement
, StatementC(..)
) where

import Control.Carrier.State.Church
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- Messages

data Message
  = Import (NonEmpty Text)


-- Statement carrier

runStatement :: ([Message] -> a -> m r) -> StatementC m a -> m r
runStatement k (StatementC m) = runState (k . reverse) [] m

newtype StatementC m a = StatementC { runStatementC :: StateC [Message] m a }
