{-# LANGUAGE TemplateHaskell #-}
module AST.Grammar.TH
( mkStaticallyKnownRuleGrammarData
) where

import           Data.Ix (Ix)
import           Data.List (mapAccumL)
import qualified Data.Set as Set
import           Foreign.Ptr
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           TreeSitter.Language (Language, languageSymbols)
import           TreeSitter.Symbol

-- | TemplateHaskell construction of a datatype for the referenced Language.
-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData :: Name -> Ptr Language -> Q [Dec]
mkStaticallyKnownRuleGrammarData name language = do
  symbols <- renameDups . map ((,) . fst <*> uncurry symbolToName) . (++ [(Regular, "ParseError")]) <$> runIO (languageSymbols language)
  Module _ modName <- thisModule
  let mkMatch symbolType str = match (conP (Name (OccName str) (NameQ modName)) []) (normalB (lift symbolType)) []
  datatype <- dataD (pure []) name [] Nothing (flip normalC [] . mkName . snd <$> symbols)
    [ derivClause Nothing (map conT [ ''Bounded, ''Enum, ''Eq, ''Ix, ''Ord, ''Show ]) ]
  symbolInstance <- [d|
    instance Symbol $(conT name) where
      symbolType = $(lamCaseE (uncurry mkMatch <$> symbols)) |]
  pure (datatype : symbolInstance)

renameDups :: [(a, String)] -> [(a, String)]
renameDups = snd . mapAccumL go mempty
  where go done (ty, name) = let name' = rename name in (Set.insert name' done, (ty, name'))
          where rename name | name `Set.member` done = rename (name ++ "'")
                            | otherwise              = name
