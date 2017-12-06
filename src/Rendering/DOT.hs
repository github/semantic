module Rendering.DOT where

import Data.Blob
import qualified Data.ByteString as B
import Data.Diff
import Data.Functor.Both
import Data.Term

renderDOTDiff :: Both Blob -> Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff _ _ = ""

renderDOTTerm :: Blob -> Term syntax ann -> B.ByteString
renderDOTTerm _ _ = ""
