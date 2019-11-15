{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, TypeOperators #-}
module Core.Parser
  ( core
  , lit
  , expr
  , record
  , comp
  ) where

-- Consult @doc/grammar.md@ for an EBNF grammar.

import           Control.Applicative
import           Control.Effect.Carrier
import           Control.Monad
import           Core.Core ((:<-) (..), Core)
import qualified Core.Core as Core
import           Core.Name
import qualified Data.Char as Char
import           Data.Foldable (foldl')
import           Data.Function
import           Data.Int
import           Data.String
import           Text.Parser.LookAhead (LookAheadParsing)
import qualified Text.Parser.Token as Token
import qualified Text.Parser.Token.Highlight as Highlight
import qualified Text.Parser.Token.Style as Style
import           Text.Trifecta hiding (ident)
import qualified Text.Trifecta.Rendering as Trifecta
import qualified Text.Trifecta.Delta as Trifecta
import qualified Source.Span as Source

-- * Identifier styles and derived parsers

newtype CoreParser m a = CoreParser { runCoreParser :: m a }
  deriving (Alternative, Applicative, CharParsing, DeltaParsing, Errable, Functor, LookAheadParsing, Monad, MonadPlus, Parsing)

instance TokenParsing m => TokenParsing (CoreParser m) where
  someSpace = Style.buildSomeSpaceParser (void (satisfy Char.isSpace)) comments
    where comments = Style.CommentStyle "" "" "//" False

type CoreParsing sig m t = ( DeltaParsing m
                           , Monad m
                           , Carrier sig t
                           , Member (Core.Ann Source.Span) sig
                           , Member Core sig
                           )

validIdentifierStart :: Char -> Bool
validIdentifierStart c = not (Char.isDigit c) && isSimpleCharacter c

coreIdents :: TokenParsing m => IdentifierStyle m
coreIdents = Token.IdentifierStyle
  { _styleName              = "core"
  , _styleStart             = satisfy validIdentifierStart
  , _styleLetter            = satisfy isSimpleCharacter
  , _styleReserved          = reservedNames
  , _styleHighlight         = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = Token.reserve coreIdents

identifier :: (TokenParsing m, Monad m, IsString s) => m s
identifier = choice [quote, plain] <?> "identifier" where
  plain = Token.ident coreIdents
  quote = between (string "#{") (symbol "}") (fromString <$> some (noneOf "{}"))

convertSpan :: Trifecta.Span -> Source.Span
convertSpan (Trifecta.Span s e _) = Source.Span (convertDelta s) (convertDelta e)
  where
    build :: Int64 -> Int64 -> Source.Pos
    build l c = Source.Pos (fromIntegral l) (fromIntegral c)
    convertDelta :: Trifecta.Delta -> Source.Pos
    convertDelta = \case
      Trifecta.Columns c _        -> build 0 c
      Trifecta.Tab x y _          -> build 0 (Trifecta.nextTab x + y)
      Trifecta.Lines l c _ _      -> build l c
      Trifecta.Directed _ l c _ _ -> build l c

positioned :: CoreParsing sig m t => m (t a) -> m (t a)
positioned act = do
  result :~ triSpan <- spanned act
  pure (Core.annAt (convertSpan triSpan) result)


-- * Parsers (corresponding to EBNF)

core :: CoreParsing sig m t => m (t Name)
core = runCoreParser expr

expr :: CoreParsing sig m t => m (t Name)
expr = positioned (ifthenelse <|> lambda <|> rec <|> load <|> assign)

assign :: CoreParsing sig m t => m (t Name)
assign = application <**> (symbolic '=' *> rhs <|> pure id) <?> "assignment"
  where rhs = flip (Core..=) <$> application

application :: CoreParsing sig m t => m (t Name)
application = projection `chainl1` (pure (Core.$$))

projection :: CoreParsing sig m t => m (t Name)
projection = foldl' (&) <$> atom <*> many (choice [ flip (Core..?)  <$ symbol ".?" <*> identifier
                                                  , flip (Core....) <$ dot         <*> identifier
                                                  ])

atom :: CoreParsing sig m t => m (t Name)
atom = positioned . choice $
  [ comp
  , lit
  , ident
  , parens expr
  ]

comp :: CoreParsing sig m t => m (t Name)
comp = braces (Core.do' <$> sepEndByNonEmpty statement semi) <?> "compound statement"

statement :: CoreParsing sig m t => m (Maybe (Named Name) :<- t Name)
statement
  =   try ((:<-) . Just <$> name <* symbol "<-" <*> expr)
  <|> (Nothing :<-) <$> expr
  <?> "statement"

ifthenelse :: CoreParsing sig m t => m (t Name)
ifthenelse = Core.if'
  <$ reserved "if"   <*> expr
  <* reserved "then" <*> expr
  <* reserved "else" <*> expr
  <?> "if-then-else statement"

rec :: CoreParsing sig m t => m (t Name)
rec = Core.rec <$ reserved "rec" <*> name <* symbolic '=' <*> expr <?> "recursive binding"

load :: CoreParsing sig m t => m (t Name)
load = Core.load <$ reserved "load" <*> expr

-- * Literals

name :: (TokenParsing m, Monad m) => m (Named Name)
name = named' <$> identifier <?> "name"

lit :: CoreParsing sig m t => m (t Name)
lit = let x `given` n = x <$ reserved n in positioned (choice
  [ Core.bool True  `given` "#true"
  , Core.bool False `given` "#false"
  , Core.unit       `given` "#unit"
  , record
  , Core.string <$> stringLiteral
  ] <?> "literal")

record :: CoreParsing sig m t => m (t Name)
record = Core.record <$ reserved "#record" <*> braces (field `sepEndBy` comma)
  where
    field = ((,) <$> identifier <* symbolic ':' <*> expr)

lambda :: CoreParsing sig m t => m (t Name)
lambda = Core.lam <$ lambduh <*> name <* arrow <*> expr <?> "lambda" where
  lambduh = symbolic 'λ' <|> symbolic '\\'
  arrow   = symbol "→"   <|> symbol "->"

ident :: CoreParsing sig m t => m (t Name)
ident = positioned (pure . namedValue <$> name <?> "identifier")
