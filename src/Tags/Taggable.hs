{-# LANGUAGE GADTs, LambdaCase, RankNTypes, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Taggable
( Definer
, Definable(..)
, defining
, runSymbolizing
, enter
, exit
, getRangeUntil
, getRange
, firstItem
)
where

import Prelude hiding (fail, filter, log)
import Prologue hiding (Element, hash)

import           Analysis.ConstructorName
import           Control.Matching hiding (target)
import           Control.Arrow hiding (first)
import           Control.Effect as Eff
import           Control.Effect.Error as Error
import qualified Control.Effect.State as State
import           Control.Monad
import           Control.Monad.Trans
import           Control.Rewriting hiding (apply)
import           Data.Abstract.Declarations
import           Data.Abstract.Name
import           Data.Blob
import           Data.History
import           Data.List (intersperse)
import           Data.Location
import           Data.Machine as Machine
import           Data.Range
import qualified Data.Source as Source
import           Data.Span
import           Data.Term
import           Data.Text hiding (empty)

import qualified Data.Syntax.Comment as Comment
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Literal as Literal
import qualified Data.Syntax.Statement as Statement
import qualified Data.Syntax.Expression as Expression
import qualified Data.Syntax.Type as Type
import qualified Data.Syntax as Syntax

 -- TODO: Move to src/Data
data Token
  = Enter Text (Maybe Range)
  | Exit Text (Maybe Range)
  | Identifier Text (Maybe Range)
  deriving (Eq, Show)

data Symbol
  = Symbol
  { name :: Text
  , kind :: Text
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  }
  deriving (Eq, Show)

data Definer a where
  Pure :: a -> Definer a
  Bind :: Definer a -> (a -> Definer b) -> Definer b
  Tell :: Token -> Definer ()
  Get :: Definer InternalState
  Put :: InternalState -> Definer ()

compile :: InternalState -> Definer a -> Machine.Plan k Token (InternalState, a)
compile p = \case
  Pure a   -> pure (p, a)
  Bind a f -> compile p a >>= (\(new, v) -> compile new (f v))
  Tell t   -> Machine.yield t $> (p, ())
  Get      -> pure (p, p)
  Put p'   -> pure (p', ())

instance Functor Definer where fmap = liftA
instance Applicative Definer where
  pure  = Pure
  (<*>) = ap
instance Monad Definer where (>>=) = Bind

enter, exit :: String -> Maybe Range -> Definer ()
enter c = Tell . Enter (pack c)
exit c = Tell . Exit (pack c)

emitIden :: Maybe Range -> Name -> Definer ()
emitIden range name = Tell (Identifier (formatName name) range)

data InternalState = InternalState
  { _source   :: Source.Source   -- We need to be able to slice
  , location  :: Location
  }
 -- InternalState handling
asks :: (InternalState -> a) -> Definer a
asks f = f <$> Get

modify :: (InternalState -> InternalState) -> Definer ()
modify f = Get >>= \x -> Put . f $! x

class (Show1 syntax, Traversable syntax, ConstructorName syntax) => Definable syntax where
  definition :: Maybe Range -> Maybe Name -> Maybe Range -> FAlgebra syntax (Definer ())
  definition r name range t = do
    let cName = constructorName t
    enter cName range
    -- maybe (pure ()) (enter "docstr" . Just) r
    maybe (pure ()) (emitIden r) name
    sequenceA_ t
    exit cName range
    -- maybe (pure ()) (exit "docstr" . Just) r

  docsMatcher ::
    ( [] :< fs
    , Declaration.Function :< fs
    , Literal.TextElement :< fs
    , Apply Functor fs
    , Apply Foldable fs
    )
    => Location -> syntax (Term (Sum fs) Location) -> Maybe Range
  docsMatcher _ _ = Nothing

  snippetRange :: Location -> syntax (Term a Location) -> Maybe Range
  snippetRange _ _ = Nothing

withLocation :: Annotated t Location => t -> Definer a -> Definer a
withLocation t act = do
  old <- asks location
  modify (\x -> x { location = annotation t })
  act <* modify (\x -> x { location = old })

defining ::
  ( Apply Functor fs
  , Apply Foldable fs
  , Apply Traversable fs
  , Apply Show1 fs
  , Apply Definable fs
  , Apply ConstructorName fs
  , Apply Declarations1 fs
  , [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  )
  => Blob
  -> Term (Sum fs) Location
  -> Machine.Source Token
defining Blob{..} term = pipe
  where pipe  = Machine.construct . fmap snd $ compile state go
        state = InternalState blobSource (termAnnotation term)
        go    = foldSubterms descend term
descend :: forall syntax fs.
  ( Definable syntax
  , Declarations (syntax (Term (Sum fs) Location))
  , Functor syntax
  , Apply Functor fs
  , Apply Foldable fs
  , [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  )
  => SubtermAlgebra syntax (Term (Sum fs) Location) (Definer ())
descend t = do
  (InternalState _ loc) <- asks id
  let n = declaredName (fmap subterm t)
  let range = docsMatcher loc (fmap subterm t)
  let r = snippetRange loc (fmap subterm t)
  definition range n r (fmap subtermRef t)

getRangeUntil ::
     Location
  -> Rule () (syntax (Term a Location)) (Term a Location)
  -> syntax (Term a Location)
  -> Maybe Range
getRangeUntil a finder r
  = let start = locationByteRange a
        end   = locationByteRange <$> rewrite (finder >>^ annotation) () r
    in either (const Nothing) (Just . subtractRange start) end

getRange matcher t
  = locationByteRange <$> runMatcher @Maybe matcher t

firstItem :: Rule env [a] a
firstItem = target >>= maybeM (Prologue.fail "empty list") . listToMaybe

type ContextToken = (Text, Maybe Range)

type Contextualizer
  = Eff (StateC [ContextToken]
  ( Eff (ErrorC TranslationError
  ( Eff VoidC))))

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module"]

contextualizing :: Blob -> Machine.ProcessT Contextualizer Token Symbol
contextualizing Blob{..} = repeatedly $ await >>= \case
  Enter x r -> enterScope (x, r)
  Exit x r  -> exitScope (x, r)
  Identifier iden rng -> lift State.get >>= \case
    ((x, r):("Context", cr):xs) | x `elem` symbolsToSummarize
      -> yield $ Symbol iden x (fmap fst xs) (slice r) (slice cr)
    ((x, r):xs) | x `elem` symbolsToSummarize
      -> yield $ Symbol iden x (fmap fst xs) (slice r) (slice rng)
    _ -> pure ()
  where
    slice = fmap (stripEnd . Source.toText . flip Source.slice blobSource)

enterScope, exitScope :: ContextToken -> Machine.PlanT k Symbol Contextualizer ()
enterScope c = lift (State.modify (c :))
exitScope  c = lift State.get >>= \case
  (x:xs) -> when (x == c) (lift (State.modify (const xs)))
  cs     -> lift (Error.throwError (UnbalancedPair c cs))

data TranslationError = UnbalancedPair ContextToken [ContextToken]
  deriving (Eq, Show)

runSymbolizing ::
  ( Apply Functor fs
  , Apply Foldable fs
  , Apply Traversable fs
  , Apply Show1 fs
  , Apply Definable fs
  , Apply ConstructorName fs
  , Apply Declarations1 fs
  , [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  )
  => Blob
  -> Term (Sum fs) Location
  -> Either TranslationError [Symbol]
runSymbolizing blob tree
  = Eff.run
  . Error.runError
  . fmap snd
  . State.runState (mempty :: [ContextToken])
  . runT $ source (defining blob tree)
      ~> contextualizing blob


docstringMatcher ::
  ( [] :< fs
  , Literal.TextElement :< fs
  , Declaration.Function :< fs
  , term ~ Term (Sum fs) Location
  )
  => Matcher term Location
docstringMatcher = match Declaration.functionBody $ do
  (a:_) <- narrow
  (Literal.TextElement _) <- guardTerm a
  pure (annotation a)

-- Instances
-- | Sums of definable terms are definable.
instance ( Apply Show1 fs, Apply Functor fs, Apply Foldable fs, Apply Traversable fs, Apply Definable fs, Apply ConstructorName fs) => Definable (Sum fs) where
  definition d n r = apply @Definable (definition d n r)
  docsMatcher a = apply @Definable (docsMatcher a)
  snippetRange x = apply @Definable (snippetRange x)

instance (Definable a) => Definable (TermF a Location) where
  definition d n r t = withLocation t (definition d n r (termFOut t))
  docsMatcher _ t = docsMatcher (termFAnnotation t) (termFOut t)
  snippetRange _ t = snippetRange (termFAnnotation t) (termFOut t)

instance Definable [] where
  definition _ _ _ = sequenceA_

instance Definable Declaration.Function where
  docsMatcher a = getRange docstringMatcher . injectTerm a
  snippetRange ann = getRangeUntil ann (arr Declaration.functionBody)

instance Definable Comment.Comment
instance Definable Expression.Times
instance Definable Expression.Plus
instance Definable Expression.Minus
instance Definable Expression.Call
instance Definable Literal.Boolean
instance Definable Literal.Integer
instance Definable Literal.TextElement
instance Definable Statement.If
instance Definable Statement.Return
instance Definable Statement.Statements
instance Definable Syntax.Context
instance Definable Syntax.Empty
instance Definable Syntax.Error
instance Definable Syntax.Identifier
instance Definable Type.Annotation
