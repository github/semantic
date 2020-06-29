{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators #-}

module Control.Effect.Sum.Project
( Project (..)
) where

import Control.Effect.Sum

class Member sub sup => Project (sub :: (* -> *) -> (* -> *)) sup where
  prj :: sup m a -> Maybe (sub m a)

instance Project sub sub where
  prj = Just

instance {-# OVERLAPPABLE #-} Project sub (sub :+: sup) where
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} Project sub sup => Project sub (sub' :+: sup) where
  prj (R g) = prj g
  prj _     = Nothing
