module Term where

import Control.Comonad.Cofree
import Syntax

type Term a annotation = Cofree (Syntax a) annotation
