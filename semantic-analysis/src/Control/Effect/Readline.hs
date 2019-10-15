{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}
module Control.Effect.Readline
( -- * Readline effect
  Readline (..)
, prompt
, print
  -- * Re-exports
, Carrier
) where

import Control.Effect.Carrier
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import GHC.Generics (Generic1)
import Prelude hiding (print)

data Readline m k
  = Prompt String (Int -> Maybe String -> m k)
  | Print (Doc AnsiStyle) (m k)
  deriving (Functor, Generic1)

instance HFunctor Readline
instance Effect   Readline


prompt :: (Member Readline sig, Carrier sig m) => String -> m (Int, Maybe String)
prompt p = send (Prompt p (curry pure))

print :: (Carrier sig m, Member Readline sig) => Doc AnsiStyle -> m ()
print s = send (Print s (pure ()))
