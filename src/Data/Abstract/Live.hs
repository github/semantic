module Data.Abstract.Live where

import Data.Abstract.Address
import Data.Set as Set

newtype Live l v = Live { unLive :: Set (Address l v) }
