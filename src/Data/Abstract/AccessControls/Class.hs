module Data.Abstract.AccessControls.Class
  ( AccessControls (..)
  , AccessControls1 (..)
  ) where

import Data.Abstract.ScopeGraph (AccessControl(..))

{-|
  The 'AccessControls' typeclass provides a mapping between a syntax and its associated 'AccessControl' type (i.e. public, protected, or private).

  Because not every syntax relates to the idea of access control, the 'termToAccessControl' method defaults to returning a 'Nothing' as the default for all syntax.

  Specialized instances should be defined per syntax when considering its 'AccessControl' is necessary for its evaluation.
-}
class AccessControls syntax where
  termToAccessControl :: syntax -> Maybe AccessControl
  termToAccessControl = const Nothing

{-|
  The 'AccessControls1' typeclass allows lifting of a function mapping a syntax to its 'AccessControl' type for rank 1 types.

  As described in the notes for the 'AccessControls' typeclass, the default for the 'liftTermToAccessControl' method is 'Nothing' for syntax terms whose evaluation does not require consideration of access control.
-}
class AccessControls1 syntax where
  liftTermToAccessControl :: (a -> Maybe AccessControl) -> syntax a -> Maybe AccessControl
  liftTermToAccessControl _ _ = Nothing
