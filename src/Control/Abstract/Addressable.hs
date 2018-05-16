{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Data.Abstract.Address
import Data.Abstract.Environment (insert)
import Data.Abstract.FreeVariables
import Data.Semigroup.Reducer
import Prologue

alloc :: Addressable location effects => Name -> Evaluator location value effects (Address location value)
alloc = fmap Address . allocLoc

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: (Addressable location effects, Members '[Resumable (AddressError location value), State (Heap location (Cell location) value)] effects) => Address location value -> Evaluator location value effects value
deref addr = do
  cell <- lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr))
  derefed <- derefCell addr cell
  maybeM (throwAddressError (UninitializedAddress addr)) derefed


data Allocator location return where
  Alloc :: Name                   -> Allocator location (Address location value)
  Deref :: Address location value -> Allocator location value


-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (Ord location, Show location) => Addressable location effects where
  derefCell :: Address location value -> Cell location value -> Evaluator location value effects (Maybe value)

  allocLoc :: Name -> Evaluator location value effects location

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Addressable location effects
                 , Members '[ Reader (Environment location value)
                            , State (Environment location value)
                            ] effects
                 )
              => Name
              -> Evaluator location value effects (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( Addressable location effects
          , Members '[ Reader (Environment location value)
                     , State (Environment location value)
                     , State (Heap location (Cell location) value)
                     ] effects
          , Reducer value (Cell location value)
          )
       => Name
       -> Evaluator location value effects value
       -> Evaluator location value effects (value, Address location value)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- localEnv (insert name addr) body
  assign addr v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( Addressable location effects
           , Members '[ Reader (Environment location value)
                      , State (Environment location value)
                      ] effects
           )
        => Name
        -> (Address location value -> Evaluator location value effects value)
        -> Evaluator location value effects value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- localEnv id (body addr)
  v <$ modifyEnv (insert name addr)


-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Addressable location effects
            , Members '[ Reader (Environment location value)
                       , Resumable (AddressError location value)
                       , Resumable (EnvironmentError value)
                       , State (Environment location value)
                       , State (Heap location (Cell location) value)
                       ] effects
            )
         => Name
         -> Evaluator location value effects value
variable name = lookupEnv name >>= maybe (freeVariableError name) deref

-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance Member Fresh effects => Addressable Precise effects where
  derefCell _ = pure . getLast . unLatest
  allocLoc _ = Precise <$> fresh

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance Member NonDet effects => Addressable Monovariant effects where
  derefCell _ cell | null cell = pure Nothing
                   | otherwise = foldMapA (pure . Just) cell
  allocLoc = pure . Monovariant

instance ( Addressable location effects
         , Members '[ Reader ModuleInfo
                    , Reader PackageInfo
                    ] effects
         )
      => Addressable (Located location) effects where
  derefCell (Address (Located loc _ _)) = raiseEff . lowerEff . derefCell (Address loc)

  allocLoc name = raiseEff (lowerEff (Located <$> allocLoc name <*> currentPackage <*> currentModule))


data AddressError location value resume where
  UnallocatedAddress :: Address location value -> AddressError location value (Cell location value)
  UninitializedAddress :: Address location value -> AddressError location value value

deriving instance Eq location => Eq (AddressError location value resume)
deriving instance Show location => Show (AddressError location value resume)
instance Show location => Show1 (AddressError location value) where
  liftShowsPrec _ _ = showsPrec
instance Eq location => Eq1 (AddressError location value) where
  liftEq _ (UninitializedAddress a) (UninitializedAddress b) = a == b
  liftEq _ (UnallocatedAddress a)   (UnallocatedAddress b)   = a == b
  liftEq _ _                        _                        = False


throwAddressError :: Member (Resumable (AddressError location value)) effects => AddressError location value resume -> Evaluator location value effects resume
throwAddressError = throwResumable

runAddressError :: Effectful (m location value) => m location value (Resumable (AddressError location value) ': effects) a -> m location value effects (Either (SomeExc (AddressError location value)) a)
runAddressError = runResumable

runAddressErrorWith :: Effectful (m location value) => (forall resume . AddressError location value resume -> m location value effects resume) -> m location value (Resumable (AddressError location value) ': effects) a -> m location value effects a
runAddressErrorWith = runResumableWith
