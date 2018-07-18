{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
module Language.TypeScript.Syntax where

import qualified Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.Package
import           Data.Abstract.Path
import           Data.Aeson
import           Data.JSON.Fields
import qualified Data.Language as Language
import qualified Data.Map as Map
import qualified Data.Text as T
import           Diffing.Algorithm
import           Prologue
import           System.FilePath.Posix
import Proto3.Suite
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode

data IsRelative = Unknown | Relative | NonRelative
  deriving (Bounded, Enum, Finite, MessageField, Named, Eq, Generic, Hashable, Ord, Show, ToJSON)

instance Primitive IsRelative where
  encodePrimitive = Encode.enum
  decodePrimitive = either (const def) id <$> Decode.enum
  primType _ = Named (Single (nameOf (Proxy @IsRelative)))

instance HasDefault IsRelative where
  def = Unknown

data ImportPath = ImportPath { unPath :: FilePath, pathIsRelative :: IsRelative }
  deriving (Eq, Generic, Hashable, Message, Named, Ord, Show, ToJSON)

instance MessageField ImportPath where
  encodeMessageField num = Encode.embedded num . encodeMessage (fieldNumber 1)
  decodeMessageField = fromMaybe def <$> Decode.embedded (decodeMessage (fieldNumber 1))
  protoType _ = messageField (Prim $ Named (Single (nameOf (Proxy @ImportPath)))) Nothing

instance HasDefault ImportPath where
  def = ImportPath mempty Relative

-- TODO: fix the duplication present in this and Python
importPath :: Text -> ImportPath
importPath str = let path = stripQuotes str in ImportPath (T.unpack path) (pathType path)
  where
    pathType xs | not (T.null xs), T.head xs == '.' = Relative -- TODO: fix partiality
                | otherwise = NonRelative

toName :: ImportPath -> Name
toName = name . T.pack . unPath

-- Node.js resolution algorithm: https://nodejs.org/api/modules.html#modules_all_together
--
-- NB: TypeScript has a couple of different strategies, but the main one (and the
-- only one we support) mimics Node.js.
resolveWithNodejsStrategy :: ( Member (Modules address) effects
                             , Member (Reader M.ModuleInfo) effects
                             , Member (Reader PackageInfo) effects
                             , Member (Resumable ResolutionError) effects
                             , Member Trace effects
                             )
                          => ImportPath
                          -> [String]
                          -> Evaluator address value effects M.ModulePath
resolveWithNodejsStrategy (ImportPath path NonRelative) exts = resolveNonRelativePath path exts
resolveWithNodejsStrategy (ImportPath path _)    exts = resolveRelativePath path exts

-- | Resolve a relative TypeScript import to a known 'ModuleName' or fail.
--
-- import { b } from "./moduleB" in /root/src/moduleA.ts
--
-- /root/src/moduleB.ts
-- /root/src/moduleB/package.json (if it specifies a "types" property)
-- /root/src/moduleB/index.ts
resolveRelativePath :: ( Member (Modules address) effects
                       , Member (Reader M.ModuleInfo) effects
                       , Member (Reader PackageInfo) effects
                       , Member (Resumable ResolutionError) effects
                       , Member Trace effects
                       )
                    => FilePath
                    -> [String]
                    -> Evaluator address value effects M.ModulePath
resolveRelativePath relImportPath exts = do
  M.ModuleInfo{..} <- currentModule
  let relRootDir = takeDirectory modulePath
  let path = joinPaths relRootDir relImportPath
  trace ("attempting to resolve (relative) require/import " <> show relImportPath)
  resolveModule path exts >>= either notFound (\x -> x <$ traceResolve relImportPath path)
  where
    notFound xs = throwResumable $ NotFoundError relImportPath xs Language.TypeScript

-- | Resolve a non-relative TypeScript import to a known 'ModuleName' or fail.
--
-- import { b } from "moduleB" in source file /root/src/moduleA.ts
--
-- /root/src/node_modules/moduleB.ts
-- /root/src/node_modules/moduleB/package.json (if it specifies a "types" property)
-- /root/src/node_modules/moduleB/index.ts
--
-- /root/node_modules/moduleB.ts, etc
-- /node_modules/moduleB.ts, etc
resolveNonRelativePath :: ( Member (Modules address) effects
                          , Member (Reader M.ModuleInfo) effects
                          , Member (Reader PackageInfo) effects
                          , Member (Resumable ResolutionError) effects
                          , Member Trace effects
                          )
                       => FilePath
                       -> [String]
                       -> Evaluator address value effects M.ModulePath
resolveNonRelativePath name exts = do
  M.ModuleInfo{..} <- currentModule
  go "." modulePath mempty
  where
    nodeModulesPath dir = takeDirectory dir </> "node_modules" </> name
    -- Recursively search in a 'node_modules' directory, stepping up a directory each time.
    go root path searched = do
      trace ("attempting to resolve (non-relative) require/import " <> show name)
      res <- resolveModule (nodeModulesPath path) exts
      case res of
        Left xs | parentDir <- takeDirectory path , root /= parentDir -> go root parentDir (searched <> xs)
                | otherwise -> notFound (searched <> xs)
        Right m -> m <$ traceResolve name m
    notFound xs = throwResumable $ NotFoundError name xs Language.TypeScript

-- | Resolve a module name to a ModulePath.
resolveModule :: ( Member (Modules address) effects
                 , Member (Reader PackageInfo) effects
                 , Member Trace effects
                 )
              => FilePath -- ^ Module path used as directory to search in
              -> [String] -- ^ File extensions to look for
              -> Evaluator address value effects (Either [FilePath] M.ModulePath)
resolveModule path' exts = do
  let path = makeRelative "." path'
  PackageInfo{..} <- currentPackage
  let packageDotJSON = Map.lookup (path </> "package.json") packageResolutions
  let searchPaths =  ((path <.>) <$> exts)
                  <> maybe mempty (:[]) packageDotJSON
                  <> (((path </> "index") <.>) <$> exts)
  trace ("searching in " <> show searchPaths)
  maybe (Left searchPaths) Right <$> resolve searchPaths

typescriptExtensions :: [String]
typescriptExtensions = ["ts", "tsx", "d.ts"]

javascriptExtensions :: [String]
javascriptExtensions = ["js"]

evalRequire :: ( AbstractValue address value effects
               , Member (Allocator address value) effects
               , Member (Env address) effects
               , Member (Modules address) effects
               )
            => M.ModulePath
            -> Name
            -> Evaluator address value effects value
evalRequire modulePath alias = letrec' alias $ \addr ->
  unit <$ makeNamespace alias addr Nothing (bindAll . fst =<< require modulePath)

data Import a = Import { importSymbols :: ![Alias], importFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

  -- http://www.typescriptlang.org/docs/handbook/module-resolution.html
instance Evaluatable Import where
  eval (Import symbols importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    importedBinds <- fst <$> require modulePath
    bindAll (renamed importedBinds)
    rvalBox unit
    where
      renamed importedBinds
        | Prologue.null symbols = importedBinds
        | otherwise = Env.aliasBindings (toTuple <$> symbols) importedBinds

data JavaScriptRequire a = JavaScriptRequire { javascriptRequireIden :: !a, javascriptRequireFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JavaScriptRequire where liftEq = genericLiftEq
instance Ord1 JavaScriptRequire where liftCompare = genericLiftCompare
instance Show1 JavaScriptRequire where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable JavaScriptRequire where
  eval (JavaScriptRequire aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath javascriptExtensions
    alias <- maybeM (throwEvalError NoNameError) (declaredName (subterm aliasTerm))
    rvalBox =<< evalRequire modulePath alias


data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportAlias :: !a, qualifiedAliasedImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedAliasedImport where liftEq = genericLiftEq
instance Ord1 QualifiedAliasedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedAliasedImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedAliasedImport where
  eval (QualifiedAliasedImport aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    alias <- maybeM (throwEvalError NoNameError) (declaredName (subterm aliasTerm))
    rvalBox =<< evalRequire modulePath alias

newtype SideEffectImport a = SideEffectImport { sideEffectImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 SideEffectImport where liftEq = genericLiftEq
instance Ord1 SideEffectImport where liftCompare = genericLiftCompare
instance Show1 SideEffectImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SideEffectImport where
  eval (SideEffectImport importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    void $ require modulePath
    rvalBox unit


-- | Qualified Export declarations
newtype QualifiedExport a = QualifiedExport { qualifiedExportSymbols :: [Alias] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedExport where liftEq = genericLiftEq
instance Ord1 QualifiedExport where liftCompare = genericLiftCompare
instance Show1 QualifiedExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedExport where
  eval (QualifiedExport exportSymbols) = do
    -- Insert the aliases with no addresses.
    for_ exportSymbols $ \Alias{..} ->
      export aliasValue aliasName Nothing
    rvalBox unit

data Alias = Alias { aliasValue :: Name, aliasName :: Name }
  deriving (Eq, Generic, Hashable, Ord, Show, Message, Named, ToJSON)

toTuple :: Alias -> (Name, Name)
toTuple Alias{..} = (aliasValue, aliasName)

-- | Qualified Export declarations that export from another module.
data QualifiedExportFrom a = QualifiedExportFrom { qualifiedExportFrom :: ImportPath, qualifiedExportFromSymbols :: ![Alias]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 QualifiedExportFrom where liftEq = genericLiftEq
instance Ord1 QualifiedExportFrom where liftCompare = genericLiftCompare
instance Show1 QualifiedExportFrom where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedExportFrom where
  eval (QualifiedExportFrom importPath exportSymbols) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    importedBinds <- fst <$> require modulePath
    -- Look up addresses in importedEnv and insert the aliases with addresses into the exports.
    for_ exportSymbols $ \Alias{..} -> do
      let address = Env.lookup aliasValue importedBinds
      maybe (throwEvalError $ ExportError modulePath aliasValue) (export aliasValue aliasName . Just) address
    rvalBox unit

newtype DefaultExport a = DefaultExport { defaultExport :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultExport where liftEq = genericLiftEq
instance Ord1 DefaultExport where liftCompare = genericLiftCompare
instance Show1 DefaultExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DefaultExport where
  eval (DefaultExport term) = do
    case declaredName term of
      Just name -> do
        addr <- lookupOrAlloc name
        v <- subtermValue term
        assign addr v
        export name name Nothing
        bind name addr
      Nothing -> throwEvalError DefaultExportError
    rvalBox unit


-- | Lookup type for a type-level key in a typescript map.
data LookupType a = LookupType { lookupTypeIdentifier :: a, lookupTypeKey :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LookupType where liftEq = genericLiftEq
instance Ord1 LookupType where liftCompare = genericLiftCompare
instance Show1 LookupType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LookupType

-- | ShorthandPropertyIdentifier used in object patterns such as var baz = { foo } to mean var baz = { foo: foo }
newtype ShorthandPropertyIdentifier a = ShorthandPropertyIdentifier { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ShorthandPropertyIdentifier where liftEq = genericLiftEq
instance Ord1 ShorthandPropertyIdentifier where liftCompare = genericLiftCompare
instance Show1 ShorthandPropertyIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ShorthandPropertyIdentifier

data Union a = Union { unionLeft :: !a, unionRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Language.TypeScript.Syntax.Union where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.Union where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.Union where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Language.TypeScript.Syntax.Union

data Intersection a = Intersection { intersectionLeft :: !a, intersectionRight :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Intersection where liftEq = genericLiftEq
instance Ord1 Intersection where liftCompare = genericLiftCompare
instance Show1 Intersection where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Intersection

data FunctionType a = FunctionType { functionTypeParameters :: !a, functionFormalParameters :: ![a], functionType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FunctionType where liftEq = genericLiftEq
instance Ord1 FunctionType where liftCompare = genericLiftCompare
instance Show1 FunctionType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable FunctionType

data AmbientFunction a = AmbientFunction { ambientFunctionContext :: ![a], ambientFunctionIdentifier :: !a, ambientFunctionParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AmbientFunction where liftEq = genericLiftEq
instance Ord1 AmbientFunction where liftCompare = genericLiftCompare
instance Show1 AmbientFunction where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AmbientFunction

data ImportRequireClause a = ImportRequireClause { importRequireIdentifier :: !a, importRequireSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportRequireClause where liftEq = genericLiftEq
instance Ord1 ImportRequireClause where liftCompare = genericLiftCompare
instance Show1 ImportRequireClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportRequireClause

newtype ImportClause a = ImportClause { importClauseElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportClause where liftEq = genericLiftEq
instance Ord1 ImportClause where liftCompare = genericLiftCompare
instance Show1 ImportClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportClause

newtype Tuple a = Tuple { tupleElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Tuple where liftEq = genericLiftEq
instance Ord1 Tuple where liftCompare = genericLiftCompare
instance Show1 Tuple where liftShowsPrec = genericLiftShowsPrec

-- This is a tuple type, not a tuple value, so we can't lean on the shared Tuple value
instance Evaluatable Tuple

data Constructor a = Constructor { constructorTypeParameters :: !a, constructorFormalParameters :: ![a], constructorType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Language.TypeScript.Syntax.Constructor where liftEq = genericLiftEq
instance Ord1 Language.TypeScript.Syntax.Constructor where liftCompare = genericLiftCompare
instance Show1 Language.TypeScript.Syntax.Constructor where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Language.TypeScript.Syntax.Constructor

data TypeParameter a = TypeParameter { typeParameter :: !a, typeParameterConstraint :: !a, typeParameterDefaultType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeParameter where liftEq = genericLiftEq
instance Ord1 TypeParameter where liftCompare = genericLiftCompare
instance Show1 TypeParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeParameter

data TypeAssertion a = TypeAssertion { typeAssertionParameters :: !a, typeAssertionExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeAssertion where liftEq = genericLiftEq
instance Ord1 TypeAssertion where liftCompare = genericLiftCompare
instance Show1 TypeAssertion where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeAssertion

newtype Annotation a = Annotation { annotationType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Annotation

newtype Decorator a = Decorator { decoratorTerm :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Decorator where liftEq = genericLiftEq
instance Ord1 Decorator where liftCompare = genericLiftCompare
instance Show1 Decorator where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Decorator

newtype ComputedPropertyName a = ComputedPropertyName { propertyName :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ComputedPropertyName where liftEq = genericLiftEq
instance Ord1 ComputedPropertyName where liftCompare = genericLiftCompare
instance Show1 ComputedPropertyName where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ComputedPropertyName

newtype Constraint a = Constraint { constraintType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Constraint where liftEq = genericLiftEq
instance Ord1 Constraint where liftCompare = genericLiftCompare
instance Show1 Constraint where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Constraint

newtype DefaultType a = DefaultType { defaultType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 DefaultType where liftEq = genericLiftEq
instance Ord1 DefaultType where liftCompare = genericLiftCompare
instance Show1 DefaultType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable DefaultType

newtype ParenthesizedType a = ParenthesizedType { parenthesizedType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ParenthesizedType where liftEq = genericLiftEq
instance Ord1 ParenthesizedType where liftCompare = genericLiftCompare
instance Show1 ParenthesizedType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ParenthesizedType

newtype PredefinedType a = PredefinedType { predefinedType :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 PredefinedType where liftEq = genericLiftEq
instance Ord1 PredefinedType where liftCompare = genericLiftCompare
instance Show1 PredefinedType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable PredefinedType

newtype TypeIdentifier a = TypeIdentifier { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeIdentifier where liftEq = genericLiftEq
instance Ord1 TypeIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeIdentifier

data NestedIdentifier a = NestedIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 NestedIdentifier where liftEq = genericLiftEq
instance Ord1 NestedIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NestedIdentifier

data NestedTypeIdentifier a = NestedTypeIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 NestedTypeIdentifier where liftEq = genericLiftEq
instance Ord1 NestedTypeIdentifier where liftCompare = genericLiftCompare
instance Show1 NestedTypeIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NestedTypeIdentifier

data GenericType a = GenericType { genericTypeIdentifier :: !a, genericTypeArguments :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 GenericType where liftEq = genericLiftEq
instance Ord1 GenericType where liftCompare = genericLiftCompare
instance Show1 GenericType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable GenericType

data TypePredicate a = TypePredicate { typePredicateIdentifier :: !a, typePredicateType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypePredicate where liftEq = genericLiftEq
instance Ord1 TypePredicate where liftCompare = genericLiftCompare
instance Show1 TypePredicate where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypePredicate

newtype ObjectType a = ObjectType { objectTypeElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ObjectType where liftEq = genericLiftEq
instance Ord1 ObjectType where liftCompare = genericLiftCompare
instance Show1 ObjectType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ObjectType

data With a = With { withExpression :: !a, withBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 With where liftEq = genericLiftEq
instance Ord1 With where liftCompare = genericLiftCompare
instance Show1 With where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable With

newtype AmbientDeclaration a = AmbientDeclaration { ambientDeclarationBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AmbientDeclaration where liftEq = genericLiftEq
instance Ord1 AmbientDeclaration where liftCompare = genericLiftCompare
instance Show1 AmbientDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AmbientDeclaration where
  eval (AmbientDeclaration body) = subtermRef body

data EnumDeclaration a = EnumDeclaration { enumDeclarationIdentifier :: !a, enumDeclarationBody :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 EnumDeclaration where liftEq = genericLiftEq
instance Ord1 EnumDeclaration where liftCompare = genericLiftCompare
instance Show1 EnumDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable EnumDeclaration

instance Declarations a => Declarations (EnumDeclaration a) where
  declaredName EnumDeclaration{..} = declaredName enumDeclarationIdentifier

newtype ExtendsClause a = ExtendsClause { extendsClauses :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ExtendsClause where liftEq = genericLiftEq
instance Ord1 ExtendsClause where liftCompare = genericLiftCompare
instance Show1 ExtendsClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ExtendsClause

newtype ArrayType a = ArrayType { arrayType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ArrayType where liftEq = genericLiftEq
instance Ord1 ArrayType where liftCompare = genericLiftCompare
instance Show1 ArrayType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ArrayType

newtype FlowMaybeType a = FlowMaybeType { flowMaybeType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 FlowMaybeType where liftEq = genericLiftEq
instance Ord1 FlowMaybeType where liftCompare = genericLiftCompare
instance Show1 FlowMaybeType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable FlowMaybeType

newtype TypeQuery a = TypeQuery { typeQuerySubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeQuery where liftEq = genericLiftEq
instance Ord1 TypeQuery where liftCompare = genericLiftCompare
instance Show1 TypeQuery where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeQuery

newtype IndexTypeQuery a = IndexTypeQuery { indexTypeQuerySubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 IndexTypeQuery where liftEq = genericLiftEq
instance Ord1 IndexTypeQuery where liftCompare = genericLiftCompare
instance Show1 IndexTypeQuery where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable IndexTypeQuery

newtype TypeArguments a = TypeArguments { typeArguments :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 TypeArguments where liftEq = genericLiftEq
instance Ord1 TypeArguments where liftCompare = genericLiftCompare
instance Show1 TypeArguments where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable TypeArguments

newtype ThisType a = ThisType { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ThisType where liftEq = genericLiftEq
instance Ord1 ThisType where liftCompare = genericLiftCompare
instance Show1 ThisType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ThisType

newtype ExistentialType a = ExistentialType { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ExistentialType where liftEq = genericLiftEq
instance Ord1 ExistentialType where liftCompare = genericLiftCompare
instance Show1 ExistentialType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ExistentialType

newtype LiteralType a = LiteralType { literalTypeSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LiteralType where liftEq = genericLiftEq
instance Ord1 LiteralType where liftCompare = genericLiftCompare
instance Show1 LiteralType where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LiteralType

data PropertySignature a = PropertySignature { modifiers :: ![a], propertySignaturePropertyName :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 PropertySignature where liftEq = genericLiftEq
instance Ord1 PropertySignature where liftCompare = genericLiftCompare
instance Show1 PropertySignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable PropertySignature

data CallSignature a = CallSignature { callSignatureTypeParameters :: !a, callSignatureParameters :: ![a], callSignatureType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 CallSignature where liftEq = genericLiftEq
instance Ord1 CallSignature where liftCompare = genericLiftCompare
instance Show1 CallSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable CallSignature

-- | Todo: Move type params and type to context
data ConstructSignature a = ConstructSignature { constructSignatureTypeParameters :: !a, constructSignatureParameters :: ![a], constructSignatureType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ConstructSignature where liftEq = genericLiftEq
instance Ord1 ConstructSignature where liftCompare = genericLiftCompare
instance Show1 ConstructSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ConstructSignature

data IndexSignature a = IndexSignature { indexSignatureSubject :: a, indexSignatureType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 IndexSignature where liftEq = genericLiftEq
instance Ord1 IndexSignature where liftCompare = genericLiftCompare
instance Show1 IndexSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable IndexSignature

data AbstractMethodSignature a = AbstractMethodSignature { abstractMethodSignatureContext :: ![a], abstractMethodSignatureName :: !a, abstractMethodSignatureParameters :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AbstractMethodSignature where liftEq = genericLiftEq
instance Ord1 AbstractMethodSignature where liftCompare = genericLiftCompare
instance Show1 AbstractMethodSignature where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable AbstractMethodSignature

data Debugger a = Debugger
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Debugger where liftEq = genericLiftEq
instance Ord1 Debugger where liftCompare = genericLiftCompare
instance Show1 Debugger where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Debugger

data ForOf a = ForOf { forOfBinding :: !a, forOfSubject :: !a, forOfBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ForOf where liftEq = genericLiftEq
instance Ord1 ForOf where liftCompare = genericLiftCompare
instance Show1 ForOf where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ForOf

data This a = This
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 This where liftEq = genericLiftEq
instance Ord1 This where liftCompare = genericLiftCompare
instance Show1 This where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable This

data LabeledStatement a = LabeledStatement { labeledStatementIdentifier :: !a, labeledStatementSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Ord1 LabeledStatement where liftCompare = genericLiftCompare
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable LabeledStatement

newtype Update a = Update { updateSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Update

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval (Module iden xs) = do
    name <- maybeM (throwEvalError NoNameError) (declaredName (subterm iden))
    rvalBox =<< letrec' name (\addr ->
      makeNamespace name addr Nothing (void (eval xs)))


data InternalModule a = InternalModule { internalModuleIdentifier :: !a, internalModuleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 InternalModule where liftEq = genericLiftEq
instance Ord1 InternalModule where liftCompare = genericLiftCompare
instance Show1 InternalModule where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InternalModule where
  eval (InternalModule iden xs) = do
    name <- maybeM (throwEvalError NoNameError) (declaredName (subterm iden))
    rvalBox =<< letrec' name (\addr ->
      makeNamespace name addr Nothing (void (eval xs)))

instance Declarations a => Declarations (InternalModule a) where
  declaredName InternalModule{..} = declaredName internalModuleIdentifier


data ImportAlias a = ImportAlias { importAliasSubject :: !a, importAlias :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImportAlias where liftEq = genericLiftEq
instance Ord1 ImportAlias where liftCompare = genericLiftCompare
instance Show1 ImportAlias where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImportAlias

data Super a = Super
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Super where liftEq = genericLiftEq
instance Ord1 Super where liftCompare = genericLiftCompare
instance Show1 Super where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Super

data Undefined a = Undefined
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 Undefined where liftEq = genericLiftEq
instance Ord1 Undefined where liftCompare = genericLiftCompare
instance Show1 Undefined where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Undefined

data ClassHeritage a = ClassHeritage { classHeritageExtendsClause :: !a, implementsClause :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ClassHeritage where liftEq = genericLiftEq
instance Ord1 ClassHeritage where liftCompare = genericLiftCompare
instance Show1 ClassHeritage where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ClassHeritage

data AbstractClass a = AbstractClass { abstractClassIdentifier :: !a,  abstractClassTypeParameters :: !a, classHeritage :: ![a], classBody :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 AbstractClass where liftEq = genericLiftEq
instance Ord1 AbstractClass where liftCompare = genericLiftCompare
instance Show1 AbstractClass where liftShowsPrec = genericLiftShowsPrec
instance Declarations a => Declarations (AbstractClass a) where
  declaredName AbstractClass{..} = declaredName abstractClassIdentifier

instance Evaluatable AbstractClass where
  eval AbstractClass{..} = do
    name <- maybeM (throwEvalError NoNameError) (declaredName (subterm abstractClassIdentifier))
    supers <- traverse subtermAddress classHeritage
    (v, addr) <- letrec name $ do
      void $ subtermValue classBody
      classBinds <- Env.head <$> getEnv
      klass name supers classBinds
    rvalBox =<< (v <$ bind name addr)


data JsxElement a = JsxElement { jsxOpeningElement :: !a,  jsxElements :: ![a], jsxClosingElement :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxElement where liftEq = genericLiftEq
instance Ord1 JsxElement where liftCompare = genericLiftCompare
instance Show1 JsxElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxElement

newtype JsxText a = JsxText { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxText where liftEq = genericLiftEq
instance Ord1 JsxText where liftCompare = genericLiftCompare
instance Show1 JsxText where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxText

newtype JsxExpression a = JsxExpression { jsxExpression :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxExpression where liftEq = genericLiftEq
instance Ord1 JsxExpression where liftCompare = genericLiftCompare
instance Show1 JsxExpression where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxExpression

data JsxOpeningElement a = JsxOpeningElement { jsxOpeningElementIdentifier :: !a,  jsxAttributes :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxOpeningElement where liftEq = genericLiftEq
instance Ord1 JsxOpeningElement where liftCompare = genericLiftCompare
instance Show1 JsxOpeningElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxOpeningElement

newtype JsxClosingElement a = JsxClosingElement { jsxClosingElementIdentifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxClosingElement where liftEq = genericLiftEq
instance Ord1 JsxClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxClosingElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxClosingElement

data JsxSelfClosingElement a = JsxSelfClosingElement { jsxSelfClosingElementIdentifier :: !a, jsxSelfClosingElementAttributes :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxSelfClosingElement where liftEq = genericLiftEq
instance Ord1 JsxSelfClosingElement where liftCompare = genericLiftCompare
instance Show1 JsxSelfClosingElement where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxSelfClosingElement

data JsxAttribute a = JsxAttribute { jsxAttributeTarget :: !a, jsxAttributeValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxAttribute where liftEq = genericLiftEq
instance Ord1 JsxAttribute where liftCompare = genericLiftCompare
instance Show1 JsxAttribute where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxAttribute

newtype ImplementsClause a = ImplementsClause { implementsClauseTypes :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 ImplementsClause where liftEq = genericLiftEq
instance Ord1 ImplementsClause where liftCompare = genericLiftCompare
instance Show1 ImplementsClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ImplementsClause

data OptionalParameter a = OptionalParameter { optionalParameterContext :: ![a], optionalParameterSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 OptionalParameter where liftEq = genericLiftEq
instance Ord1 OptionalParameter where liftCompare = genericLiftCompare
instance Show1 OptionalParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable OptionalParameter

data RequiredParameter a = RequiredParameter { requiredParameterContext :: ![a], requiredParameterSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 RequiredParameter where liftEq = genericLiftEq
instance Ord1 RequiredParameter where liftCompare = genericLiftCompare
instance Show1 RequiredParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable RequiredParameter

data RestParameter a = RestParameter { restParameterContext :: ![a], restParameterSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 RestParameter where liftEq = genericLiftEq
instance Ord1 RestParameter where liftCompare = genericLiftCompare
instance Show1 RestParameter where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable RestParameter

newtype JsxFragment a = JsxFragment { terms :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxFragment where liftEq = genericLiftEq
instance Ord1 JsxFragment where liftCompare = genericLiftCompare
instance Show1 JsxFragment where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxFragment

data JsxNamespaceName a = JsxNamespaceName { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Message1, Named1, Ord, Show, ToJSONFields1, Traversable)

instance Eq1 JsxNamespaceName where liftEq = genericLiftEq
instance Ord1 JsxNamespaceName where liftCompare = genericLiftCompare
instance Show1 JsxNamespaceName where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable JsxNamespaceName
