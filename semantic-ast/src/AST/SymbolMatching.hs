{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.SymbolMatching
  ( SymbolMatching (..),
  )
where

import AST.Token
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import TreeSitter.Node (Node)

class SymbolMatching (a :: * -> *) where
  matchedSymbols :: Proxy a -> [Int]

  -- | Provide error message describing the node symbol vs. the symbols this can match
  showFailure :: Proxy a -> Node -> String

instance SymbolMatching f => SymbolMatching (M1 i c f) where
  matchedSymbols _ = matchedSymbols (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

instance SymbolMatching f => SymbolMatching (Rec1 f) where
  matchedSymbols _ = matchedSymbols (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

instance (KnownNat n, KnownSymbol sym) => SymbolMatching (Token sym n) where
  matchedSymbols _ = [fromIntegral (natVal (Proxy @n))]
  showFailure _ _ = "expected " ++ symbolVal (Proxy @sym)

instance (SymbolMatching f, SymbolMatching g) => SymbolMatching (f :+: g) where
  matchedSymbols _ = matchedSymbols (Proxy @f) <> matchedSymbols (Proxy @g)
  showFailure _ = sep <$> showFailure (Proxy @f) <*> showFailure (Proxy @g)

instance SymbolMatching f => SymbolMatching (shape :.: f) where
  matchedSymbols _ = matchedSymbols (Proxy @f)
  showFailure _ = showFailure (Proxy @f)

sep :: String -> String -> String
sep a b = a ++ ". " ++ b
