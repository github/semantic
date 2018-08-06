module Control.Rule.Engine.Builtin where

import Control.Rule
import Control.Monad.Reader
import Data.Reprinting.Token

checkSeparators :: Rule Token Token
checkSeparators = fromMealy "Builtin: check separators" $
  current <$> ask

{-

checkSeparators :: Rule Token Token
checkSeparators = withPrevious "Builtin: check separator" $
  inListContext $ do

    ensure





-}
