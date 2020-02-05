{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module AST.GenerateSyntax
( syntaxDatatype
, astDeclarationsForLanguage
) where

import           AST.Deserialize (Children (..), Datatype (..), DatatypeName (..), Field (..), Multiple (..), Named (..), Required (..), Type (..))
import           AST.Token
import           AST.Traversable1.Class
import qualified AST.Unmarshal as TS
import           Data.Aeson hiding (String)
import           Data.Foldable
import           Data.List
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
  input <- runIO (eitherDecodeFileStrict' invocationRelativePath) >>= either fail pure
  allSymbols <- runIO (getAllSymbols language)
  debugSymbolNames <- [d|
    debugSymbolNames :: [String]
    debugSymbolNames = $(listE (map (litE . stringL . debugPrefix) allSymbols))
    |]
  (debugSymbolNames <>) . concat @[] <$> traverse (syntaxDatatype language allSymbols) input

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

-- Auto-generate Haskell datatypes for sums, products and leaf types
syntaxDatatype :: Ptr TS.Language -> [(String, Named)] -> Datatype -> Q [Dec]
syntaxDatatype language allSymbols datatype = skipDefined $ do
  typeParameterName <- newName "a"
  case datatype of
    SumType (DatatypeName _) _ subtypes -> do
      types' <- fieldTypesToNestedSum subtypes
      let fieldName = mkName ("get" <> nameStr)
      con <- recC name [TH.varBangType fieldName (TH.bangType strictness (pure types' `appT` varT typeParameterName))]
      hasFieldInstance <- makeHasFieldInstance (conT name) (varT typeParameterName) (varE fieldName)
      traversalInstances <- makeTraversalInstances (conT name)
      pure
        (  NewtypeD [] name [PlainTV typeParameterName] Nothing con [deriveGN, deriveStockClause, deriveAnyClassClause]
        :  hasFieldInstance
        <> traversalInstances)
    ProductType (DatatypeName datatypeName) named children fields -> do
      con <- ctorForProductType datatypeName typeParameterName children fields
      symbolMatchingInstance <- symbolMatchingInstance allSymbols name named datatypeName
      traversalInstances <- makeTraversalInstances (conT name)
      pure
        (  generatedDatatype name [con] typeParameterName
        :  symbolMatchingInstance
        <> traversalInstances)
      -- Anonymous leaf types are defined as synonyms for the `Token` datatype
    LeafType (DatatypeName datatypeName) Anonymous -> do
      tsSymbol <- runIO $ withCStringLen datatypeName (\(s, len) -> TS.ts_language_symbol_for_name language s len False)
      pure [ TySynD name [] (ConT ''Token `AppT` LitT (StrTyLit datatypeName) `AppT` LitT (NumTyLit (fromIntegral tsSymbol))) ]
    LeafType (DatatypeName datatypeName) Named -> do
      con <- ctorForLeafType (DatatypeName datatypeName) typeParameterName
      symbolMatchingInstance <- symbolMatchingInstance allSymbols name Named datatypeName
      traversalInstances <- makeTraversalInstances (conT name)
      pure
        (  generatedDatatype name [con] typeParameterName
        :  symbolMatchingInstance
        <> traversalInstances)
  where
    -- Skip generating datatypes that have already been defined (overridden) in the module where the splice is running.
    skipDefined m = do
      isLocal <- lookupTypeName nameStr >>= maybe (pure False) isLocalName
      if isLocal then pure [] else m
    name = mkName nameStr
    nameStr = toNameString (datatypeNameStatus datatype) (getDatatypeName (AST.Deserialize.datatypeName datatype))
    deriveStockClause = DerivClause (Just StockStrategy) [ ConT ''Eq, ConT ''Ord, ConT ''Show, ConT ''Generic, ConT ''Generic1]
    deriveAnyClassClause = DerivClause (Just AnyclassStrategy) [ConT ''TS.Unmarshal, ConT ''Traversable1 `AppT` VarT (mkName "someConstraint")]
    deriveGN = DerivClause (Just NewtypeStrategy) [ConT ''TS.SymbolMatching]
    generatedDatatype name cons typeParameterName = DataD [] name [PlainTV typeParameterName] Nothing cons [deriveStockClause, deriveAnyClassClause]


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

makeHasFieldInstance :: TypeQ -> TypeQ -> ExpQ -> Q [Dec]
makeHasFieldInstance ty param elim =
  [d|instance HasField "ann" $(ty `appT` param) $param where
      getField = TS.gann . $elim |]

-- | Create TH-generated SymbolMatching instances for sums, products, leaves
symbolMatchingInstance :: [(String, Named)] -> Name -> Named -> String -> Q [Dec]
symbolMatchingInstance allSymbols name named str = do
  let tsSymbols = elemIndices (str, named) allSymbols
      names = intercalate ", " $ fmap (debugPrefix . (!!) allSymbols) tsSymbols
  [d|instance TS.SymbolMatching $(conT name) where
      matchedSymbols _   = tsSymbols
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
ctorForProductType :: String -> Name -> Maybe Children -> [(String, Field)] -> Q Con
ctorForProductType constructorName typeParameterName children fields = ctorForTypes constructorName lists where
  lists = annotation : fieldList <> childList
  annotation = ("ann", varT typeParameterName)
  fieldList = map (fmap toType) fields
  childList = toList $ fmap toTypeChild children
  toType (MkField required fieldTypes mult) =
    let ftypes = fieldTypesToNestedSum fieldTypes `appT` varT typeParameterName
    in case (required, mult) of
      (Required, Multiple) -> appT (conT ''NonEmpty) ftypes
      (Required, Single)   -> ftypes
      (Optional, Multiple) -> appT (conT ''[]) ftypes
      (Optional, Single)   -> appT (conT ''Maybe) ftypes
  toTypeChild (MkChildren field) = ("extra_children", toType field)

-- | Build Q Constructor for leaf types (nodes with no fields or subtypes)
ctorForLeafType :: DatatypeName -> Name -> Q Con
ctorForLeafType (DatatypeName name) typeParameterName = ctorForTypes name
  [ ("ann",  varT typeParameterName) -- ann :: a
  , ("text", conT ''Text)            -- text :: Text
  ]

-- | Build Q Constructor for records
ctorForTypes :: String -> [(String, Q TH.Type)] -> Q Con
ctorForTypes constructorName types = recC (toName Named constructorName) recordFields where
  recordFields = map (uncurry toVarBangType) types
  toVarBangType str type' = TH.varBangType (mkName . toHaskellCamelCaseIdentifier $ str) (TH.bangType strictness type')


-- | Convert field types to Q types
fieldTypesToNestedSum :: NonEmpty AST.Deserialize.Type -> Q TH.Type
fieldTypesToNestedSum xs = go (toList xs)
  where
    combine lhs rhs = (conT ''(:+:) `appT` lhs) `appT` rhs -- (((((a :+: b) :+: c) :+: d)) :+: e)   ((a :+: b) :+: (c :+: d))
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
