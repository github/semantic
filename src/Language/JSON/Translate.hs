module Language.JSON.Translate where

import qualified Control.Monad.Effect.Exception as Exc
import           Data.Language
import           Data.Reprinting.Token
import           Prologue
import           Reprinting.Translate

instance Translation 'JSON where
  translation _ content context = case (content, context) of
    (Fragment f, _) -> emit $ splice f
    (Truth t, _)    -> emit . splice $ if t then "true" else "false"
    (Nullity, _)    -> emit . splice $ "null"

    (Open, List:_)         -> emit $ splice "["
    (Open, Associative:_)  -> emit $ splice "{"

    (Close, List:_)        -> emit $ splice "]"
    (Close, Associative:_) -> emit $ splice "}"

    (Separator, List:_)       -> emit $ splice ","
    (Separator, Associative:_) -> emit $ splice ":"
    _ -> Exc.throwError (Unexpected "invalid context")
