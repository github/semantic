module Assigning.Assignment.Deterministic where

import qualified Data.Set as Set
import Prologue

class (Applicative (f s), Ord s, Show s) => Assigning f s where
  (<!>) :: f s a -> f s a -> f s a
  infixl 3 <!>

  sym :: s -> f s s

combine :: Ord s => Bool -> Set s -> Set s -> Set s
combine e s1 s2 = if e then s1 <> s2 else lowerBound

type Input s = [s]

data DetPar s a = DetPar
  { isNullable :: Bool
  , firstSet   :: Set s
  , match      :: Input s -> Set s -> (Input s, a)
  }
  deriving (Functor)

instance Ord s => Applicative (DetPar s) where
  pure a = DetPar Prelude.True lowerBound (\ inp _ -> (inp, a))
  DetPar n1 f1 p1 <*> ~(DetPar n2 f2 p2) = DetPar (n1 && n2) (combine n1 f1 f2) (p1 `pseq` p2)
    where p1 `pseq` p2 = \ inp follow ->
            let (inp1, v1) = p1 inp (combine n2 f2 follow)
                (inp2, v2) = p2 inp1 follow
                res = v1 v2
            in  res `seq` (inp2, res)

instance (Ord s, Show s) => Assigning DetPar s where
  DetPar n1 f1 p1 <!> DetPar n2 f2 p2 = DetPar (n1 || n2) (f1 <> f2) (p1 `palt` p2)
    where p1 `palt` p2 = p
            where p [] follow =
                    if      n1 then p1 [] follow
                    else if n2 then p2 [] follow
                    else error "unexpected eof"
                  p inp@(s:_) follow =
                    if      s `Set.member` f1 then p1 inp follow
                    else if s `Set.member` f2 then p2 inp follow
                    else if n1 && s `Set.member` follow then p1 inp follow
                    else if n2 && s `Set.member` follow then p2 inp follow
                    else error ("illegal input symbol: " <> show s)

  sym s = DetPar Prelude.False (Set.singleton s) (\ (_:inp) _ -> (inp, s))

invokeDet :: DetPar s a -> Input s -> a
invokeDet (DetPar _ _ p) inp = snd (p inp lowerBound)
