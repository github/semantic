module Renderer.TOC where

import Renderer
import Data.Record
import Info

toc :: (HasField fields Category, HasField fields Cost, HasField fields Range) => Renderer (Record fields)
toc sources diff = TOCOutput []