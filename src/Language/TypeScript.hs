{-# LANGUAGE DataKinds #-}
module Language.TypeScript where

import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Data.Foldable (toList)
import Data.Source
import Data.Text (Text)
import Info
import Language
import qualified Syntax as S
import Term

