{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TypeOperators #-}
module Marshal.Examples () where

import Control.Effect.Reader
import Control.Monad.Fail
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics ((:+:), Generic1, Generic)
import Numeric (readDec)
import Prelude hiding (fail)
import Source.Range
import TreeSitter.Token
import TreeSitter.Unmarshal

-- | An example of a sum-of-products datatype.
data Expr a
  = IfExpr (If a)
  | BlockExpr (Block a)
  | VarExpr (Var a)
  | LitExpr (Lit a)
  | BinExpr (Bin a)
  deriving (Generic1, Unmarshal)

-- | Product with multiple fields.
data If a = If { ann :: a, condition :: Expr a, consequence :: Expr a, alternative :: Maybe (Expr a) }
  deriving (Generic1, Unmarshal)

instance SymbolMatching If where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Single-field product.
data Block a = Block { ann :: a, body :: [Expr a] }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Block where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Leaf node.
data Var a = Var { ann :: a, text :: Text.Text }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Var where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Custom leaf node.
data Lit a = Lit { ann :: a, lit :: IntegerLit }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Lit where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Product with anonymous sum field.
data Bin a = Bin { ann :: a, lhs :: Expr a, op :: (AnonPlus :+: AnonTimes) a, rhs :: Expr a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Bin where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Anonymous leaf node.
type AnonPlus = Token "+" 0

-- | Anonymous leaf node.
type AnonTimes = Token "*" 1


newtype IntegerLit = IntegerLit Integer
  deriving (Generic, ToJSON)

instance UnmarshalAnn IntegerLit where
  unmarshalAnn node = do
    Range start end <- unmarshalAnn node
    bytestring <- ask
    let drop = B.drop start
        take = B.take (end - start)
        slice = take . drop
        str = Text.unpack (Text.decodeUtf8 (slice bytestring))
    case readDec str of
      (i, _):_ -> pure (IntegerLit i)
      _        -> fail ("could not parse '" <> str <> "'")
