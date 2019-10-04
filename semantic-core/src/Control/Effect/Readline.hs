{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, RankNTypes, TypeApplications #-}
module Control.Effect.Readline
( Readline(..)
, AnyDoc(..)
, prompt
, print
, println
, askLine
, Line(..)
, increment
) where

import Control.Carrier
import Data.Int
import Data.String
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic1)
import Prelude hiding (print)

data Readline m k
  = Prompt String (Maybe String -> m k)
  | Print AnyDoc (m k)
  | AskLine (Line -> m k)
  deriving (Functor, Generic1)

instance HFunctor Readline
instance Effect   Readline

newtype AnyDoc = AnyDoc { unAnyDoc :: forall a . Doc a }

prompt :: (IsString str, Has Readline sig m) => String -> m (Maybe str)
prompt p = fmap fromString <$> send (Prompt p pure)

print :: (Pretty a, Has Readline sig m) => a -> m ()
print s = send (Print (AnyDoc (pretty s)) (pure ()))

println :: (Pretty a, Has Readline sig m) => a -> m ()
println s = print s >> print @String "\n"

askLine :: Has Readline sig m => m Line
askLine = send (AskLine pure)

newtype Line = Line Int64

increment :: Line -> Line
increment (Line n) = Line (n + 1)
