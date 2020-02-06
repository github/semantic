{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TypeOperators #-}
module AST.Grammar.Examples () where

import Control.Effect.Reader
import Control.Monad.Fail
import qualified Data.ByteString as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics ((:+:), Generic1)
import Numeric (readDec)
import Prelude hiding (fail)
import Source.Range
import AST.Token
import AST.Unmarshal

-- | An example of a sum-of-products datatype.
newtype Expr a = Expr ((If :+: Block :+: Var :+: Lit :+: Bin) a)
  deriving (Generic1, Unmarshal)

instance SymbolMatching Expr where
  matchedSymbols _ = []
  showFailure _ _ = ""

-- | Product with multiple fields.
data If a = If { ann :: a, condition :: Expr a, consequence :: Expr a, alternative :: Maybe (Expr a) }
  deriving (Generic1, Unmarshal)

instance SymbolMatching If where
  matchedSymbols _ = []
  showFailure _ _ = ""

-- | Single-field product.
data Block a = Block { ann :: a, body :: [Expr a] }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Block where
  matchedSymbols _ = []
  showFailure _ _ = ""

-- | Leaf node.
data Var a = Var { ann :: a, text :: Text.Text }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Var where
  matchedSymbols _ = []
  showFailure _ _ = ""

-- | Custom leaf node.
data Lit a = Lit { ann :: a, lit :: IntegerLit }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Lit where
  matchedSymbols _ = []
  showFailure _ _ = ""

-- | Product with anonymous sum field.
data Bin a = Bin { ann :: a, lhs :: Expr a, op :: (AnonPlus :+: AnonTimes) a, rhs :: Expr a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Bin where
  matchedSymbols _ = []
  showFailure _ _ = ""

-- | Anonymous leaf node.
type AnonPlus = Token "+" 0

-- | Anonymous leaf node.
type AnonTimes = Token "*" 1


newtype IntegerLit = IntegerLit Integer

instance UnmarshalAnn IntegerLit where
  unmarshalAnn node = do
    Range start end <- unmarshalAnn node
    bytestring <- asks source
    let drop = B.drop start
        take = B.take (end - start)
        slice = take . drop
        str = Text.unpack (Text.decodeUtf8 (slice bytestring))
    case readDec str of
      (i, _):_ -> pure (IntegerLit i)
      _        -> fail ("could not parse '" <> str <> "'")
