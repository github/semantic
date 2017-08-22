{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Functor.Classes.Pretty.Orphans
( module Pretty
) where

import Data.ByteString
import Data.Functor.Classes.Pretty as Pretty
import Data.Text.Encoding

instance Pretty ByteString where
  pretty = pretty . decodeUtf8With (\ _ -> ('\xfffd' <$))
