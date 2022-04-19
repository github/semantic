{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module AST.GenerateSyntax
( syntaxDatatype
, astDeclarationsForLanguage
, astDeclarationsIO
) where

import           AST.Deserialize
    (Children (..), Datatype (..), DatatypeName (..), Field (..), Multiple (..), Named (..), Required (..), Type (..))
import qualified AST.Parse as Parse
import           AST.Token
import           AST.Traversable1.Class
import qualified AST.Unmarshal as TS
import           Data.Aeson hiding (String)
import           Data.Foldable
import           Data.List (elemIndices, genericIndex, intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import           Foreign.C.String
import           Foreign.Ptr
import           GHC.Generics hiding (Constructor, Datatype)
import           GHC.Records
import           Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax as TH
import           System.Directory
import           System.FilePath.Posix
import qualified TreeSitter.Language as TS
import           TreeSitter.Node
import           TreeSitter.Symbol (TSSymbol, toHaskellCamelCaseIdentifier, toHaskellPascalCaseIdentifier)

-- | Derive Haskell datatypes from a language and its @node-types.json@ file.
--
-- Datatypes will be generated according to the specification in the @node-types.json@ file, with anonymous leaf types defined as synonyms for the 'Token' datatype.
--
-- Any datatypes among the node types which have already been defined in the module where the splice is run will be skipped, allowing customization of the representation of parts of the tree. Note that this should be used sparingly, as it imposes extra maintenance burden, particularly when the grammar is changed. This may be used to e.g. parse literals into Haskell equivalents (e.g. parsing the textual contents of integer literals into 'Integer's), and may require defining 'TS.UnmarshalAnn' or 'TS.SymbolMatching' instances for (parts of) the custom datatypes, depending on where and how the datatype occurs in the generated tree, in addition to the usual 'Foldable', 'Functor', etc. instances provided for generated datatypes.
astDeclarationsForLanguage :: Ptr TS.Language -> FilePath -> Q [Dec]
astDeclarationsForLanguage language filePath = do
  _ <- TS.addDependentFileRelative filePath
  currentFilename <- loc_filename <$> location
  pwd             <- runIO getCurrentDirectory
  let invocationRelativePath = takeDirectory (pwd </> currentFilename) </> filePath
  astDeclarationsRelative lookupTypeName language invocationRelativePath

astDeclarationsIO :: Ptr TS.Language -> FilePath -> IO [Dec]
astDeclarationsIO lang p = runQ (astDeclarationsRelative (const (pure Nothing)) lang p)

astDeclarationsRelative :: (String -> Q (Maybe Name)) -> Ptr TS.Language -> FilePath -> Q [Dec]
astDeclarationsRelative lookupName language invocationRelativePath = do
  input <- runIO (eitherDecodeFileStrict' invocationRelativePath) >>= either fail pure
  allSymbols <- runIO (getAllSymbols language)
  debugSymbolNames <- [d|
    debugSymbolNames :: [String]
    debugSymbolNames = $(listE (map (litE . stringL . debugPrefix) allSymbols))
    |]
  mappend debugSymbolNames . concat @[] <$> traverse (syntaxDatatype lookupName language allSymbols) input

-- Build a list of all symbols
getAllSymbols :: Ptr TS.Language -> IO [(String, Named)]
getAllSymbols language = do
  count <- TS.ts_language_symbol_count language
  traverse getSymbol [(0 :: TSSymbol) .. fromIntegral (pred count)]
  where
    getSymbol i = do
      cname <- TS.ts_language_symbol_name language i
      n <- peekCString cname
      t <- TS.ts_language_symbol_type language i
      let named = if t == 0 then Named else Anonymous
      pure (n, named)

annParameterName :: Name
annParameterName = mkName "a"

-- Auto-generate Haskell datatypes for sums, products and leaf types
syntaxDatatype :: (String -> Q (Maybe Name)) -> Ptr TS.Language -> [(String, Named)] -> Datatype -> Q [Dec]
syntaxDatatype lookupType language allSymbols datatype = skipDefined $ do
  let traversalInstances = mappend <$> makeStandaloneDerivings (conT name) <*> makeTraversalInstances (conT name)
      glue a b c = a : b <> c
      name = mkName nameStr
      generatedDatatype cons = dataD (cxt []) name [plainTV annParameterName] Nothing cons [deriveStockClause, deriveAnyClassClause]
      deriveStockClause = derivClause (Just StockStrategy) [conT ''Generic, conT ''Generic1]
      deriveAnyClassClause = derivClause (Just AnyclassStrategy) [ [t| (forall a. Traversable1 a) |] ]
      deriveGN = derivClause (Just NewtypeStrategy) [conT ''TS.SymbolMatching]
  case datatype of
    SumType (DatatypeName _) _ subtypes ->
      let types' = fieldTypesToNestedSum subtypes
          fieldName = mkName ("get" <> nameStr)
          con = recC name [varBangType fieldName (bangType strictness (types' `appT` varT annParameterName))]
          hasFieldInstance = makeHasFieldInstance (conT name) (varE fieldName)
          newType = newtypeD (cxt []) name [plainTV annParameterName] Nothing con [deriveGN, deriveStockClause, deriveAnyClassClause]
      in glue <$> newType <*> hasFieldInstance <*> traversalInstances
    ProductType datatypeName named children fields ->
      let con = ctorForProductType datatypeName children fields
          symbols = symbolMatchingInstance allSymbols name named datatypeName
      in glue <$> generatedDatatype [con] <*> symbols <*> traversalInstances
      -- Anonymous leaf types are defined as synonyms for the `Token` datatype
    LeafType (DatatypeName datatypeName) Anonymous -> do
      let tsSymbol = runIO $ withCStringLen datatypeName (\(s, len) -> TS.ts_language_symbol_for_name language s len False)
      fmap (pure @[]) (tySynD name [] (conT ''Token `appT` litT (strTyLit datatypeName) `appT` litT (tsSymbol >>= numTyLit . fromIntegral)))
    LeafType datatypeName Named ->
      let con = ctorForLeafType datatypeName annParameterName
          symbols = symbolMatchingInstance allSymbols name Named datatypeName
      in glue <$> generatedDatatype [con] <*> symbols <*> traversalInstances
  where
    -- Skip generating datatypes that have already been defined (overridden) in the module where the splice is running.
    skipDefined m = do
      isLocal <- lookupType nameStr >>= maybe (pure False) isLocalName
      if isLocal then pure [] else m
    nameStr = toNameString (datatypeNameStatus datatype) (getDatatypeName (AST.Deserialize.datatypeName datatype))

makeStandaloneDerivings :: TypeQ -> Q [Dec]
makeStandaloneDerivings ty =
  [d|
   deriving instance (Eq a) => Eq ($ty a)
   deriving instance (Ord a) => Ord ($ty a)
   deriving instance (Show a) => Show ($ty a)
   instance TS.Unmarshal ($ty)
   |]

makeTraversalInstances :: TypeQ -> Q [Dec]
makeTraversalInstances ty =
  [d|
  instance Foldable $ty where
    foldMap = foldMapDefault1
  instance Functor $ty where
    fmap = fmapDefault1
  instance Traversable $ty where
    traverse = traverseDefault1
  |]

makeHasFieldInstance :: TypeQ -> ExpQ -> Q [Dec]
makeHasFieldInstance ty elim =
  [d|instance HasField "ann" ($ty a) a where
      getField = TS.gann . $elim |]

-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: [(String, Named)] -> Name -> Named -> DatatypeName -> Q [Dec]
symbolMatchingInstance allSymbols name named (DatatypeName str) = do
  let tsSymbols = elemIndices (str, named) allSymbols
      names = intercalate ", " $ fmap (debugPrefix . (!!) allSymbols) tsSymbols
  [d|instance TS.SymbolMatching $(conT name) where
      matchedSymbols _   = $(lift tsSymbols)
      showFailure _ node = "expected " <> $(litE (stringL names))
                        <> " but got " <> if nodeSymbol node == 65535 then "ERROR" else genericIndex debugSymbolNames (nodeSymbol node)
                        <> " [" <> show r1 <> ", " <> show c1 <> "] -"
                        <> " [" <> show r2 <> ", " <> show c2 <> "]"
        where TSPoint r1 c1 = nodeStartPoint node
              TSPoint r2 c2 = nodeEndPoint node|]

-- | Prefix symbol names for debugging to disambiguate between Named and Anonymous nodes.
debugPrefix :: (String, Named) -> String
debugPrefix (name, Named)     = name
debugPrefix (name, Anonymous) = "_" <> name

-- | Build Q Constructor for product types (nodes with fields)
ctorForProductType :: DatatypeName -> Maybe Children -> [(String, Field)] -> Q Con
ctorForProductType constructorName children fields = ctorForTypes constructorName lists where
  lists = annotation : fieldList <> childList
  annotation = ("ann", varT annParameterName)
  fieldList = map (fmap (toType)) fields
  childList = toList $ fmap toTypeChild children

  inject t = conT ''Parse.Err `appT` t

  toType :: Field -> TypeQ
  toType (MkField required fieldTypes mult) =
    let ftypes = inject (fieldTypesToNestedSum fieldTypes `appT` varT annParameterName)
    in case (required, mult) of
      (Required, Multiple) -> appT (conT ''NonEmpty) ftypes
      (Required, Single)   -> ftypes
      (Optional, Multiple) -> appT listT ftypes
      (Optional, Single)   -> appT (conT ''Maybe) ftypes

  toTypeChild (MkChildren field) = ("extra_children", toType field)

-- | Build Q Constructor for leaf types (nodes with no fields or subtypes)
ctorForLeafType :: DatatypeName -> Name -> Q Con
ctorForLeafType name annParameterName = ctorForTypes name
  [ ("ann",  varT annParameterName) -- ann :: a
  , ("text", conT ''Text)            -- text :: Text
  ]

-- TODO: clarify the paths in ctorForProductType, ctorForLeafType, and ctorForTypes,
-- inserting an appropriate (''f `appT`) thing

-- | Build Q Constructor for records
ctorForTypes :: DatatypeName -> [(String, Q TH.Type)] -> Q Con
ctorForTypes (DatatypeName constructorName) types = recC (toName Named constructorName) recordFields
  where
    recordFields = map (uncurry toVarBangType) types
    toVarBangType str type' = TH.varBangType (mkName . toHaskellCamelCaseIdentifier $ str) (TH.bangType strictness type')


-- | Convert field types to Q types
fieldTypesToNestedSum :: NonEmpty AST.Deserialize.Type -> Q TH.Type
fieldTypesToNestedSum xs = go (toList xs)
  where
    combine lhs rhs = uInfixT lhs ''(:+:) rhs -- (((((a :+: b) :+: c) :+: d)) :+: e)   ((a :+: b) :+: (c :+: d))
    convertToQType (MkType (DatatypeName n) named) = conT (toName named n)
    go [x] = convertToQType x
    go xs  = let (l,r) = splitAt (length xs `div` 2) xs in combine (go l) (go r)


-- | Create bang required to build records
strictness :: BangQ
strictness = TH.bang noSourceUnpackedness noSourceStrictness

-- | Prepend "Anonymous" to named node when false, otherwise use regular toName
toName :: Named -> String -> Name
toName named str = mkName (toNameString named str)

toNameString :: Named -> String -> String
toNameString named str = prefix named <> toHaskellPascalCaseIdentifier str
  where
    prefix Anonymous = "Anonymous"
    prefix Named     = ""

-- | Get the 'Module', if any, for a given 'Name'.
moduleForName :: Name -> Maybe Module
moduleForName n = Module . PkgName <$> namePackage n <*> (ModName <$> nameModule n)

-- | Test whether the name is defined in the module where the splice is executed.
isLocalName :: Name -> Q Bool
isLocalName n = (moduleForName n ==) . Just <$> thisModule
