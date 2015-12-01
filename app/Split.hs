module Split (split) where

import Diff
import Rainbow

split :: Diff a Info -> String -> String -> IO ByteString
split diff a b = return mempty
