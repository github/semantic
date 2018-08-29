{-# LANGUAGE TypeOperators, GADTs, PolyKinds #-}
module Data.Abstract.ScopeGraph (concat, prepend)where

import Prelude hiding (concat)

import Data.Finite

type Scope k = Finite k

data (––>) s s' where
    Empty :: s ––> s
    -- TODO: `s` here was previously `Member s' (EdgesOf s)`
    Cons  :: Scope s -> Scope s' ––> Scope s'' -> Scope s ––> Scope s''

concat :: Scope s ––> Scope s' -> Scope s' ––> Scope s'' -> Scope s ––> Scope s''
concat Empty s2 = s2
concat (Cons a s1) s2 = (Cons a (concat s1 s2))

data (|>) s name where
    Path :: Scope s ––> Scope s' -> name -> Scope s |> name

prepend :: Scope s ––> Scope s' -> Scope s' |> name  -> Scope s |> name
prepend p (Path p' name) = Path (concat p p') name
