module Tags.Taggable.Precise
( Python(..)
) where

import qualified TreeSitter.Python.AST as Python

newtype Python a = Python { getPython :: Python.Module a }
