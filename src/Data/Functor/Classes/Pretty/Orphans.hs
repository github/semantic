{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Functor.Classes.Pretty.Orphans
( module Pretty
) where

import Data.ByteString
import Data.Functor.Classes.Pretty as Pretty
import Data.Text.Encoding
import GHC.Stack

instance Pretty ByteString where
  pretty = pretty . decodeUtf8With (\ _ -> ('\xfffd' <$))

instance Pretty SrcLoc where
  pretty SrcLoc{..}
    | srcLocStartLine == srcLocEndLine
    , srcLocStartCol == srcLocEndCol
    = pretty srcLocFile <> colon <> pretty srcLocStartLine <> colon <> pretty srcLocStartCol
    | srcLocStartLine == srcLocEndLine
    = pretty srcLocFile <> colon <> pretty srcLocStartLine <> colon <> pretty srcLocStartCol <> pretty '-' <> pretty srcLocEndCol
    | otherwise
    = pretty srcLocFile <> colon <> pretty srcLocStartLine <> colon <> pretty srcLocStartCol <> pretty '-' <> pretty srcLocEndLine <> colon <> pretty srcLocEndCol
