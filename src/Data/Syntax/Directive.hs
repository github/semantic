{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Syntax.Directive (module Data.Syntax.Directive) where

import           Data.Abstract.Evaluatable
import           Data.Abstract.Module (ModuleInfo (..))
import           Data.Functor.Classes.Generic
import           Data.Hashable.Lifted
import qualified Data.Text as T
import           GHC.Generics (Generic1)
import           Source.Span
import qualified System.Path as Path

-- A file directive like the Ruby constant `__FILE__`.
data File a = File
  deriving (Declarations1, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Traversable)

instance Eq1 File where liftEq = genericLiftEq
instance Ord1 File where liftCompare = genericLiftCompare
instance Show1 File where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable File where
  eval _ _ File = currentModule >>= string . T.pack . Path.toString . modulePath


-- A line directive like the Ruby constant `__LINE__`.
data Line a = Line
  deriving (Declarations1, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Traversable)

instance Eq1 Line where liftEq = genericLiftEq
instance Ord1 Line where liftCompare = genericLiftCompare
instance Show1 Line where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Line where
  eval _ _ Line = currentSpan >>= integer . fromIntegral . line . start
