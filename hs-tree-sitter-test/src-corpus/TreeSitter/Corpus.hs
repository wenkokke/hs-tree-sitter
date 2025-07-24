{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TreeSitter.Corpus (
  TestCase (..),
  nameString,
  codeString,
  sexpString,
  readCorpusFile,
) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.IO qualified as TLIO
import GHC.Exception (Exception (..))
import Text.Printf (printf)

-- The name of each test is written between two lines containing only = (equal sign) characters.
-- Then the input source code is written, followed by a line containing three or more - (dash) characters.
-- Then, the expected output syntax tree is written as an S-expression. The exact placement of whitespace in the S-expression doesn’t matter, but ideally the syntax tree should be legible. Note that the S-expression does not show syntax nodes like func, ( and ;, which are expressed as strings and regexes in the grammar. It only shows the named nodes, as described in this section of the page on parser usage.
-- The expected output section can also optionally show the field names associated with each child node. To include field names in your tests, you write a node’s field name followed by a colon, before the node itself in the S-expression:

data TestCase = TestCase
  { name :: !T.Text
  , code :: !TL.Text
  , sexp :: !TL.Text
  }
  deriving (Show)

nameString :: TestCase -> String
nameString = T.unpack . (.name)

codeString :: TestCase -> String
codeString = TL.unpack . (.code)

sexpString :: TestCase -> String
sexpString = TL.unpack . (.sexp)

data Stream e a
  = Nil
  | a :>: Stream e a
  | Err e

data Next
  = Name
  | Code
  | SExp
  deriving (Show)

data SNext (st :: Next) :: Type where
  SName :: SNext 'Name
  SCode :: SNext 'Code
  SSExp :: SNext 'SExp

fromSNext :: SNext st -> Next
fromSNext SName = Name
fromSNext SCode = Code
fromSNext SSExp = SExp

type family StName (st :: Next) :: Type where
  StName 'Name = ()
  StName 'Code = T.Text
  StName 'SExp = T.Text

type family StCode (st :: Next) :: Type where
  StCode 'Name = ()
  StCode 'Code = TLB.Builder
  StCode 'SExp = TLB.Builder

type family StSExp (st :: Next) :: Type where
  StSExp 'Name = ()
  StSExp 'Code = ()
  StSExp 'SExp = TLB.Builder

data CorpusParserState
  = forall (st :: Next).
  CPS
  { file :: !FilePath
  , line :: !Int
  , next :: SNext st
  , name :: StName st
  , code :: StCode st
  , sexp :: StSExp st
  }

data CorpusParserError
  = CPE
  { file :: !FilePath
  , line :: !Int
  , next :: Next
  }
  deriving (Show)

instance Exception CorpusParserError where
  displayException CPE{..} =
    printf "Parse Error at %s:%d. Expected %s" file line $
      case next of
        Name -> "line with '==='"
        Code -> "line with '---'"
        SExp -> "line with '==='"

readCorpusFile :: FilePath -> IO (Either String [TestCase])
readCorpusFile fp = streamToList <$> readCorpusFileToStream fp
 where
  streamToList :: (Exception e) => Stream e a -> Either String [a]
  streamToList = \case
    Nil -> pure []
    x :>: xs -> (x :) <$> streamToList xs
    Err e -> Left (displayException e)

readCorpusFileToStream :: FilePath -> IO (Stream CorpusParserError TestCase)
readCorpusFileToStream fp = parse start . TL.lines <$> TLIO.readFile fp
 where
  start :: CorpusParserState
  start = CPS fp 1 SName () () ()

  parse :: CorpusParserState -> [TL.Text] -> Stream CorpusParserError TestCase
  parse CPS{..} = case next of
    SName -> \case
      [] -> Nil
      (l1 : ls)
        | isBlank l1 ->
            parse CPS{line = line + 1, ..} ls
      (l1 : l2 : l3 : ls)
        | isEqFence l1 && isEqFence l3 ->
            parse
              CPS
                { line = line + 1
                , next = SCode
                , name = TL.toStrict l2
                , code = TLB.fromString []
                , ..
                }
              ls
      _otherwise -> Err CPE{next = fromSNext next, ..}
    SCode -> \case
      (l : ls)
        | isDashFence l ->
            parse
              CPS
                { line = line + 1
                , next = SSExp
                , name = name
                , code = code
                , sexp = TLB.fromString []
                , ..
                }
              ls
        | otherwise ->
            parse
              CPS
                { line = line + 1
                , code = code <> TLB.fromLazyText l <> TLB.fromString "\n"
                , ..
                }
              ls
      _otherwise -> Err CPE{next = fromSNext next, ..}
    SSExp -> \case
      [] -> testCase :>: Nil
      (l : ls)
        | isEqFence l ->
            testCase
              :>: parse
                CPS
                  { line = line + 1
                  , next = SName
                  , name = ()
                  , code = ()
                  , sexp = ()
                  , ..
                  }
                (l : ls)
        | otherwise ->
            parse
              CPS
                { line = line + 1
                , sexp = sexp <> TLB.fromLazyText l
                , ..
                }
              ls
     where
      testCase = TestCase (T.strip name) (TL.strip $ TLB.toLazyText code) (TL.strip $ TLB.toLazyText sexp)

  isBlank :: TL.Text -> Bool
  isBlank l = TL.null l || TL.all (== ' ') l

  isEqFence :: TL.Text -> Bool
  isEqFence l = not (TL.null l) && TL.all (== '=') l

  isDashFence :: TL.Text -> Bool
  isDashFence l = TL.length (TL.takeWhile (== '-') l) >= 3
