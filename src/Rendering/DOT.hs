module Rendering.DOT where

import qualified Data.ByteString as B
import Data.Diff
import Data.Term

renderDOTDiff :: Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff _ = ""

renderDOTTerm :: Term syntax ann -> B.ByteString
renderDOTTerm _ = ""
