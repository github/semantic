module Syntax where

import Data.Map

data Syntax f a =
	Leaf a
	| Indexed [f]
	| Fixed [f]
	| Keyed (Map String f)
