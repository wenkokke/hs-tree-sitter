{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TreeSitter.SExp (
  Symbol (..),
  FieldName (..),
  SExp (..),
  loose,
  prettySExp,
  prettySExpDiff,
  parseSExp,
  prettyParseError,
) where

import Control.Applicative (Alternative (..), asum)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, Pretty (..), dquotes, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String (renderString)
import Text.Parsec (ParseError, SourceName)
import Text.Parsec qualified as P
import Text.Parsec.Text (Parser)

#if MIN_VERSION_parsec(3,1,17)
import Control.Exception (Exception (..))
#endif

type Ident = Text

pIdent :: Parser Ident
pIdent = T.pack <$> ((:) <$> P.letter <*> many (P.letter <|> P.digit <|> P.char '_'))

newtype Symbol = Symbol {unSymbol :: Ident}
  deriving (Eq, Show)
  deriving newtype (Pretty)

pSymbol :: Parser Symbol
pSymbol = Symbol <$> pIdent <* P.notFollowedBy (P.char ':')

newtype FieldName = FieldName {unFieldName :: Ident}
  deriving (Eq, Show)
  deriving newtype (Pretty)

pFieldName :: Parser FieldName
pFieldName = FieldName <$> pIdent <* P.char ':'

data SExp
  = Atom Symbol
  | List [SExp]
  | Field FieldName SExp
  | String Text
  deriving (Eq, Show)

pSExp :: Parser SExp
pSExp =
  P.spaces
    *> asum
      [ Atom <$> P.try pSymbol <* P.spaces
      , List <$> P.between (P.char '(' <* P.spaces) (P.char ')' <* P.spaces) (P.many1 pSExp)
      , Field <$> pFieldName <* P.spaces <*> pSExp
      , String <$> pString
      ]

pString :: Parser Text
pString = P.between (P.char '"') (P.char '"') (T.concat <$> many (pNotQuote <|> pEscapedQuote))
 where
  pNotQuote :: Parser Text
  pNotQuote = T.singleton <$> P.satisfy (/= '"')
  pEscapedQuote :: Parser Text
  pEscapedQuote = T.pack "\\\"" <$ P.char '\\' <* P.char '"'

parseSExp :: SourceName -> Text -> Either ParseError SExp
parseSExp = P.runParser pSExp ()

prettyParseError :: ParseError -> String
#if MIN_VERSION_parsec(3,1,17)
prettyParseError = displayException
#else
prettyParseError = show
#endif

loose :: SExp -> SExp
loose sexp@(Atom _) = sexp
loose (List sexps) = List (loose <$> sexps)
loose (Field _name sexp) = loose sexp
loose sexp@(String _) = sexp

instance Pretty SExp where
  pretty :: SExp -> Doc ann
  pretty = \case
    Atom symbol -> pretty symbol
    List sexps -> PP.parens (PP.nest 2 (PP.vsep (pretty <$> sexps)))
    Field name sexp -> pretty name <> pretty ":" <+> pretty sexp
    String text -> dquotes (pretty text)

prettySExp :: SExp -> String
prettySExp = renderString . PP.layoutSmart PP.defaultLayoutOptions . pretty

prettySExpDiff :: SExp -> SExp -> String
prettySExpDiff sexp1 sexp2 =
  ppDiff $ getGroupedDiff (lines . prettySExp $ sexp1) (lines . prettySExp $ sexp2)
