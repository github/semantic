{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Unmarshal
( parseByteString
, UnmarshalState(..)
, UnmarshalError(..)
, FieldName(..)
, Unmarshal(..)
, UnmarshalAnn(..)
, UnmarshalField(..)
, SymbolMatching(..)
, Match(..)
, hoist
, lookupSymbol
, unmarshalNode
, GHasAnn(..)
) where

import           AST.Token as TS
import           AST.Parse
import           Control.Carrier.Reader
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Coerce
import           Data.Foldable (toList)
import           Data.Functor.Identity
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Data.Text.Encoding.Error (lenientDecode)
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           GHC.Records
import           GHC.TypeLits
import           Source.Loc
import           Source.Span
import           TreeSitter.Cursor as TS
import           TreeSitter.Language as TS
import           TreeSitter.Node as TS
import           TreeSitter.Parser as TS
import           TreeSitter.Tree as TS

-- Parse source code and produce AST
parseByteString :: (Unmarshal t, UnmarshalAnn a) => Ptr TS.Language -> ByteString -> IO (Either String (t a))
parseByteString language bytestring = withParser language $ \ parser -> withParseTree parser bytestring $ \ treePtr ->
  if treePtr == nullPtr then
    pure (Left "error: didn't get a root node")
  else
    withRootNode treePtr $ \ rootPtr ->
      withCursor (castPtr rootPtr) $ \ cursor ->
        (Right <$> runReader (UnmarshalState bytestring cursor) (liftIO (peek rootPtr) >>= unmarshalNode))
          `catch` (pure . Left . getUnmarshalError)

newtype UnmarshalError = UnmarshalError { getUnmarshalError :: String }
  deriving (Show)

instance Exception UnmarshalError

data UnmarshalState = UnmarshalState
  { source :: {-# UNPACK #-} !ByteString
  , cursor :: {-# UNPACK #-} !(Ptr Cursor)
  }

type MatchM = ReaderC UnmarshalState IO

newtype Match t = Match
  { runMatch :: forall a . UnmarshalAnn a => Node -> MatchM (t a)
  }

-- | A church-encoded binary tree with constant-time 'singleton', 'mempty', '<>', and 'fmap', and linear-time 'foldMap'.
newtype B a = B (forall r . (r -> r -> r) -> (a -> r) -> r -> r)

instance Functor B where
  fmap f (B run) = B (\ fork leaf -> run fork (leaf . f))
  {-# INLINE fmap #-}
  a <$ B run = B (\ fork leaf -> run fork (leaf . const a))
  {-# INLINE (<$) #-}

instance Semigroup (B a) where
  B l <> B r = B (\ fork leaf nil -> fork (l fork leaf nil) (r fork leaf nil))
  {-# INLINE (<>) #-}

instance Monoid (B a) where
  mempty = B (\ _ _ nil -> nil)
  {-# INLINE mempty #-}

instance Foldable B where
  foldMap f (B run) = run (<>) f mempty
  {-# INLINE foldMap #-}

singleton :: a -> B a
singleton a = B (\ _ leaf _ -> leaf a)
{-# INLINE singleton #-}

hoist :: (forall x . t x -> t' x) -> Match t -> Match t'
hoist f (Match run) = Match (fmap f . run)
{-# INLINE hoist #-}

lookupSymbol :: TSSymbol -> IntMap.IntMap a -> Maybe a
lookupSymbol sym map = IntMap.lookup (fromIntegral sym) map
{-# INLINE lookupSymbol #-}

-- | Unmarshal a node
unmarshalNode :: forall t a .
                 ( UnmarshalAnn a
                 , Unmarshal t
                 )
  => Node
  -> MatchM (t a)
unmarshalNode node = case lookupSymbol (nodeSymbol node) matchers' of
  Just t  -> runMatch t node
  Nothing -> liftIO . throwIO . UnmarshalError $ showFailure (Proxy @t) node
{-# INLINE unmarshalNode #-}

-- | Unmarshalling is the process of iterating over tree-sitter’s parse trees using its tree cursor API and producing Haskell ASTs for the relevant nodes.
--
--   Datatypes which can be constructed from tree-sitter parse trees may use the default definition of 'matchers' providing that they have a suitable 'Generic1' instance.
class SymbolMatching t => Unmarshal t where
  matchers' :: IntMap.IntMap (Match t)
  matchers' = IntMap.fromList (toList matchers)

  matchers :: B (Int, Match t)
  default matchers :: (Generic1 t, GUnmarshal (Rep1 t)) => B (Int, Match t)
  matchers = foldMap (singleton . (, match)) (matchedSymbols (Proxy @t))
    where match = Match $ \ node -> do
            cursor <- asks cursor
            goto cursor (nodeTSNode node)
            fmap to1 (gunmarshalNode node)

instance (Unmarshal f, Unmarshal g) => Unmarshal (f :+: g) where
  matchers = fmap (fmap (hoist L1)) matchers <> fmap (fmap (hoist R1)) matchers

instance (Applicative shape, Unmarshal f) => Unmarshal (shape :.: f) where
  matchers = let base = matchers @f in fmap (fmap promote) base
    where
      promote (Match f) = Match (fmap (fmap (Comp1 . pure)) f)

instance Unmarshal t => Unmarshal (Rec1 t) where
  matchers = coerce (matchers @t)

instance (KnownNat n, KnownSymbol sym) => Unmarshal (Token sym n) where
  matchers = singleton (fromIntegral (natVal (Proxy @n)), Match (fmap Token . unmarshalAnn))


-- | Unmarshal an annotation field.
--
--   Leaf nodes have 'Text.Text' fields, and leaves, anonymous leaves, and products all have parametric annotation fields. All of these fields are unmarshalled using the metadata of the node, e.g. its start/end bytes, without reference to any child nodes it may contain.
class UnmarshalAnn a where
  unmarshalAnn
    :: Node
    -> MatchM a

instance UnmarshalAnn () where
  unmarshalAnn _ = pure ()

instance UnmarshalAnn Text.Text where
  unmarshalAnn node = do
    range <- unmarshalAnn node
    asks (decodeUtf8With lenientDecode . slice range . source)

-- | Instance for pairs of annotations
instance (UnmarshalAnn a, UnmarshalAnn b) => UnmarshalAnn (a,b) where
  unmarshalAnn node = (,)
    <$> unmarshalAnn @a node
    <*> unmarshalAnn @b node

instance UnmarshalAnn Loc where
  unmarshalAnn node = Loc
    <$> unmarshalAnn @Range node
    <*> unmarshalAnn @Span  node

instance UnmarshalAnn Range where
  unmarshalAnn node = do
    let start = fromIntegral (nodeStartByte node)
        end   = fromIntegral (nodeEndByte node)
    pure (Range start end)

instance UnmarshalAnn Span where
  unmarshalAnn node = do
    let spanStart = pointToPos (nodeStartPoint node)
        spanEnd   = pointToPos (nodeEndPoint node)
    pure (Span spanStart spanEnd)

pointToPos :: TSPoint -> Pos
pointToPos (TSPoint line column) = Pos (fromIntegral line) (fromIntegral column)


-- | Optional/repeated fields occurring in product datatypes are wrapped in type constructors, e.g. 'Maybe', '[]', or 'NonEmpty', and thus can unmarshal zero or more nodes for the same field name.
class UnmarshalField t where
  unmarshalField
    :: ( Unmarshal f
       , UnmarshalAnn ann
       )
    => String -- ^ datatype name
    -> String -- ^ field name
    -> [Node] -- ^ nodes
    -> MatchM (t (f ann))

instance UnmarshalField Err where
  unmarshalField _ _ [] = pure $ Fail "No items provided to unmarshalField."
  unmarshalField _ _ [x] = Success <$> unmarshalNode x
  unmarshalField d f _ = pure $ Fail ("type '" <> d <> "' expected zero or one nodes in field '" <> f <> "' but got multiple")

instance UnmarshalField Maybe where
  unmarshalField _ _ []  = pure Nothing
  unmarshalField _ _ [x] = Just <$> unmarshalNode x
  unmarshalField d f _   = liftIO . throwIO . UnmarshalError $ "type '" <> d <> "' expected zero or one nodes in field '" <> f <> "' but got multiple"

instance UnmarshalField Identity where
  unmarshalField _ _ [x] = Identity <$> unmarshalNode x
  unmarshalField d f _   = liftIO . throwIO . UnmarshalError $ "type '" <> d <> "' expected zero or one nodes in field '" <> f <> "' but got multiple"

instance UnmarshalField [] where
  unmarshalField d f (x:xs) = do
    head' <- unmarshalNode x
    tail' <- unmarshalField d f xs
    pure $ head' : tail'
  unmarshalField _ _ [] = pure []

instance UnmarshalField NonEmpty where
  unmarshalField d f (x:xs) = do
    head' <- unmarshalNode x
    tail' <- unmarshalField d f xs
    pure $ head' :| tail'
  unmarshalField d f [] = liftIO . throwIO . UnmarshalError $ "type '" <> d <> "' expected one or more nodes in field '" <> f <> "' but got zero"

class SymbolMatching (sym :: * -> *) where
  matchedSymbols :: Proxy sym -> [Int]

  -- | Provide error message describing the node symbol vs. the symbols this can match
  showFailure :: Proxy sym -> Node -> String

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

-- | Move the cursor to point at the passed 'TSNode'.
goto :: Ptr Cursor -> TSNode -> MatchM ()
goto cursor node = liftIO (with node (ts_tree_cursor_reset_p cursor))


type Fields = [(FieldName, Node)]

-- | Return the fields remaining in the current branch, represented as 'Map.Map' of 'FieldName's to their corresponding 'Node's.
getFields :: Ptr Cursor -> Node -> MatchM Fields
getFields cursor node
  | maxCount == 0 = pure []
  | otherwise     = do
    nodes <- liftIO . allocaArray maxCount $ \ ptr -> do
      actualCount <- ts_tree_cursor_copy_child_nodes cursor ptr
      peekArray (fromIntegral actualCount) ptr
    traverse (\ node -> (, node) <$> getFieldName node) nodes
  where
  maxCount = fromIntegral (nodeChildCount node)
  getFieldName node
    | nodeFieldName node == nullPtr = pure (FieldName "extraChildren")
    | otherwise                     = FieldName . toHaskellCamelCaseIdentifier <$> liftIO (peekCString (nodeFieldName node))

lookupField :: FieldName -> Fields -> [Node]
lookupField k = map snd . filter ((== k) . fst)


-- | Return a 'ByteString' that contains a slice of the given 'ByteString'.
slice :: Range -> ByteString -> ByteString
slice (Range start end) = take . drop
  where drop = B.drop start
        take = B.take (end - start)


newtype FieldName = FieldName { getFieldName :: String }
  deriving (Eq, Ord, Show)

-- | Generic construction of ASTs from a 'Map.Map' of named fields.
--
--   Product types (specifically, record types) are constructed by looking up the node for each corresponding field name in the map, moving the cursor to it, and then invoking 'unmarshalNode' to construct the value for that field. Leaf types are constructed as a special case of product types.
--
--   Sum types are constructed by using the current node’s symbol to select the corresponding constructor deterministically.
class GUnmarshal f where
  gunmarshalNode
    :: UnmarshalAnn ann
    => Node
    -> MatchM (f ann)

instance (Datatype d, GUnmarshalData f) => GUnmarshal (M1 D d f) where
  gunmarshalNode = go (gunmarshalNode' (datatypeName @d undefined)) where
    go :: (Node -> MatchM (f ann)) -> Node -> MatchM (M1 i c f ann)
    go = coerce

instance (GUnmarshal f, Applicative shape) => GUnmarshal (shape :.: f) where
  gunmarshalNode = fmap (Comp1 . pure) . gunmarshalNode @f

class GUnmarshalData f where
  gunmarshalNode'
    :: UnmarshalAnn ann
    => String
    -> Node
    -> MatchM (f ann)

instance GUnmarshalData f => GUnmarshalData (M1 i c f) where
  gunmarshalNode' = go gunmarshalNode' where
    go :: (String -> Node -> MatchM (f a)) -> String -> Node -> MatchM (M1 i c f a)
    go = coerce

-- For anonymous leaf nodes:
instance GUnmarshalData U1 where
  gunmarshalNode' _ _ = pure U1

-- For unary products:
instance UnmarshalAnn k => GUnmarshalData (K1 c k) where
  gunmarshalNode' _ = go unmarshalAnn where
    go :: (Node -> MatchM k) -> Node -> MatchM (K1 c k a)
    go = coerce

-- For anonymous leaf nodes
instance GUnmarshalData Par1 where
  gunmarshalNode' _ = go unmarshalAnn where
    go :: (Node -> MatchM a) -> Node -> MatchM (Par1 a)
    go = coerce

instance Unmarshal t => GUnmarshalData (Rec1 t) where
  gunmarshalNode' _ = go unmarshalNode where
    go :: (Node -> MatchM (t a)) -> Node -> MatchM (Rec1 t a)
    go = coerce

-- For product datatypes:
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshalData (f :*: g) where
  gunmarshalNode' datatypeName node = asks cursor >>= flip getFields node >>= gunmarshalProductNode @(f :*: g) datatypeName node


-- | Generically unmarshal products
class GUnmarshalProduct f where
  gunmarshalProductNode
    :: UnmarshalAnn ann
    => String
    -> Node
    -> Fields
    -> MatchM (f ann)

-- Product structure
instance (GUnmarshalProduct f, GUnmarshalProduct g) => GUnmarshalProduct (f :*: g) where
  gunmarshalProductNode datatypeName node fields = (:*:)
    <$> gunmarshalProductNode @f datatypeName node fields
    <*> gunmarshalProductNode @g datatypeName node fields

-- Contents of product types (ie., the leaves of the product tree)
instance UnmarshalAnn k => GUnmarshalProduct (M1 S c (K1 i k)) where
  gunmarshalProductNode _ node _ = go unmarshalAnn node where
    go :: (Node -> MatchM k) -> Node -> MatchM (M1 S c (K1 i k) a)
    go = coerce

instance GUnmarshalProduct (M1 S c Par1) where
  gunmarshalProductNode _ node _ = go unmarshalAnn node where
    go :: (Node -> MatchM a) -> Node -> MatchM (M1 S c Par1 a)
    go = coerce

instance (UnmarshalField f, Unmarshal g, Selector c) => GUnmarshalProduct (M1 S c (f :.: g)) where
  gunmarshalProductNode datatypeName _ = go (unmarshalField datatypeName fieldName . lookupField (FieldName fieldName)) where
    go :: (Fields -> MatchM (f (g a))) -> Fields -> MatchM (M1 S c (f :.: g) a)
    go = coerce
    fieldName = selName @c undefined

instance (Unmarshal t, Selector c) => GUnmarshalProduct (M1 S c (Rec1 t)) where
  gunmarshalProductNode datatypeName _ fields =
    case lookupField (FieldName fieldName) fields of
      []  -> liftIO . throwIO . UnmarshalError $ "type '" <> datatypeName <> "' expected a node '" <> fieldName <> "' but didn't get one"
      [x] -> go unmarshalNode x where
        go :: (Node -> MatchM (t a)) -> Node -> MatchM (M1 S c (Rec1 t) a)
        go = coerce
      _   -> liftIO . throwIO . UnmarshalError $ "type '" <> datatypeName <> "' expected a node but got multiple"
    where
    fieldName = selName @c undefined


class GHasAnn ann t where
  gann :: t ann -> ann

instance GHasAnn ann f => GHasAnn ann (M1 i c f) where
  gann = gann . unM1

instance (GHasAnn ann l, GHasAnn ann r) => GHasAnn ann (l :+: r) where
  gann (L1 l) = gann l
  gann (R1 r) = gann r

instance {-# OVERLAPPABLE #-} HasField "ann" (t ann) ann => GHasAnn ann t where
  gann = getField @"ann"
