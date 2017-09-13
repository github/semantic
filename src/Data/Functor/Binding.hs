{-# LANGUAGE DerivingStrategies, GADTs, GeneralizedNewtypeDeriving, NoStrictData, RankNTypes #-}
module Data.Functor.Binding
( Metavar(..)
-- Abstract binding trees
, BindingF(..)
, bindings
, freeMetavariables
, maxBoundMetavariable
, letBind
-- Environments
, Env(..)
, envExtend
, envLookup
) where

import Data.Aeson (KeyValue(..), ToJSON(..), object, pairs)
import Data.Foldable (fold)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (fold)
import Data.JSON.Fields
import Data.Semigroup
import qualified Data.Set as Set

-- | A metavariable, represented as an 'Int'. These should only be constructed via the 'letBind' helper in order to avoid accidentally shadowing bound metavariables.
--
--   This is a *meta*variable in that it represents an extra-syntactic variable used within the context of some structure which may have its own orthogonal notion of variables which do not interact. For example, 'BindingF' and 'Metavar' can be used to provide templating for an AST without worrying about shadowing the AST’s own variables.
newtype Metavar = Metavar Int
  deriving (Eq, Ord, Show)
  deriving newtype (Enum, ToJSON)


-- | A functor adding let-bindings and variable references to some other functorial structure.
--
--   Typically this will be used in concert with a recursive, 'Fix'-like structure, resulting in abstract binding trees à la Harper.
data BindingF f recur
  -- | 'Let' represents the binding of zero or more variables (given by the 'Env' field) in a scope (given by the @f recur@ field).
  = Let (Env recur) (f recur)
  -- | A reference to a variable defined in an enclosing 'Let'.
  | Var Metavar
  deriving (Foldable, Functor, Traversable)

-- | Produce the variables bound by a given node. This operates nonrecursively, and thus does not return all variables in scope at a given point in a tree.
bindings :: BindingF f recur -> Env recur
bindings (Let vars _) = vars
bindings _            = mempty


-- | Compute the set of free 'Metavar'iables in a given 'Recursive' tree.
freeMetavariables :: (Foldable syntax, Functor syntax, Recursive t, Base t ~ BindingF syntax) => t -> Set.Set Metavar
freeMetavariables = cata $ \ diff -> case diff of
  Let vars body -> fold vars <> foldr Set.delete (fold body) (fst <$> unEnv vars)
  Var v -> Set.singleton v

-- | Compute the maximum bound 'Metavar'iable. Note that this assumes that 'Let's contain only smaller bindings, which is the invariant maintained by 'letBind'.
maxBoundMetavariable :: (Foldable syntax, Functor syntax, Recursive t, Base t ~ BindingF syntax) => t -> Maybe Metavar
maxBoundMetavariable = cata $ \ diff -> case diff of
  Let vars _ -> foldMaxMap (Just . fst) (unEnv vars)
  Var _ -> Nothing

-- | Compute the maximum value of a possibly-empty structure by mapping elements into some 'Ord'ered values.
foldMaxMap :: (Foldable t, Ord b) => (a -> Maybe b) -> t a -> Maybe b
foldMaxMap f = foldr (max . f) Nothing


-- | Construct a scope binding a variable to a value using a higher-order function to construct the body of the 'Let'.
letBind :: (Foldable syntax, Functor syntax, Corecursive t, Recursive t, Base t ~ BindingF syntax) => t -> (Metavar -> syntax t) -> t
letBind diff f = embed (Let (Env [(n, diff)]) body)
  where body = f n
        n = maybe (Metavar 0) succ (foldMaxMap maxBoundMetavariable body)


newtype Env a = Env { unEnv :: [(Metavar, a)] }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

envExtend :: Metavar -> a -> Env a -> Env a
envExtend var val (Env m) = Env ((var, val) : m)

envLookup :: Metavar -> Env a -> Maybe a
envLookup var = lookup var . unEnv


instance Eq1 f => Eq1 (BindingF f) where
  liftEq eq (Let v1 b1) (Let v2 b2) = liftEq eq v1 v2 && liftEq eq b1 b2
  liftEq _  (Var v1)    (Var v2)    = v1 == v2
  liftEq _  _           _           = False

instance (Eq1 f, Eq a) => Eq (BindingF f a) where
  (==) = eq1

instance Eq1 Env where
  liftEq eq (Env v1) (Env v2) = liftEq (liftEq eq) v1 v2


instance Show1 f => Show1 (BindingF f) where
  liftShowsPrec sp sl d (Let vars body) = showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Let" d vars body
  liftShowsPrec _  _  d (Var var)       = showsUnaryWith showsPrec "Var" d var

instance (Show1 f, Show a) => Show (BindingF f a) where
  showsPrec = showsPrec1

instance Show1 Env where
  liftShowsPrec sp sl d (Env vs) = showsUnaryWith (const (liftShowList sp sl)) "Env" d vs


instance ToJSONFields1 f => ToJSONFields1 (BindingF f) where
  toJSONFields1 (Let vars body) = [ "vars" .= unEnv vars ] <> toJSONFields1 body
  toJSONFields1 (Var v)         = [ "metavar" .= v ]

instance (ToJSONFields1 f, ToJSON a) => ToJSONFields (BindingF f a) where
  toJSONFields = toJSONFields1

instance (ToJSON a, ToJSONFields1 f) => ToJSON (BindingF f a) where
  toJSON = object . toJSONFields1
  toEncoding = pairs . mconcat . toJSONFields1
