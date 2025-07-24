{-|
Tests for `TreeSitter.CApi`.
-}
module Test.TreeSitter.CApi where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Foldable (for_)
import Foreign (castPtr, nullPtr)
import Foreign.C (withCStringLen)
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import TreeSitter.CApi (
  TSStateId (..),
  ts_language_delete,
  ts_language_state_count,
  ts_lookahead_iterator_current_symbol,
  ts_lookahead_iterator_delete,
  ts_lookahead_iterator_new,
  ts_lookahead_iterator_next,
  ts_parser_delete,
  ts_parser_new,
  ts_parser_parse_string,
 )
import TreeSitter.While (tree_sitter_while)

-- | All tests for `TreeSitter.CApi`.
tests :: TestTree
tests =
  testGroup
    "CApi"
    [ test_ts_lookaheaditerator
    , test_ts_language_delete
    , test_ts_parser_parse_fails_without_language
    ]

-- | Does the lookahead iterator work?
test_ts_lookaheaditerator :: TestTree
test_ts_lookaheaditerator =
  testCase "test_ts_lookaheaditerator" $ do
    language <- castConstPtr <$> tree_sitter_while
    stateCount <- ts_language_state_count language
    for_ [0 .. stateCount - 1] $ \stateId -> do
      lookaheadIterator <- ts_lookahead_iterator_new language (TSStateId . fromIntegral $ stateId)
      let loop = do
            _currentSymbol <- ts_lookahead_iterator_current_symbol (ConstPtr lookaheadIterator)
            success <- ts_lookahead_iterator_next lookaheadIterator
            when (success >= 1) loop
      loop
      ts_lookahead_iterator_delete lookaheadIterator

-- | Is deleting a @`TreeSitter.CApi.TSLanguage`@ multiple times safe?
test_ts_language_delete :: TestTree
test_ts_language_delete =
  testCase "ts_language_delete" $ do
    language <- castConstPtr <$> tree_sitter_while
    ts_language_delete language
    ts_language_delete language

-- | Does @`ts_parser_parse_string`@ return @NULL@ when the parser does not have a language assigned.
test_ts_parser_parse_fails_without_language :: TestTree
test_ts_parser_parse_fails_without_language =
  testCase "test_ts_parser_parse fails without language" $ do
    bracket ts_parser_new ts_parser_delete $ \ts_parser ->
      withCStringLen "" $ \(string, string_length) -> do
        tree_p <-
          ts_parser_parse_string
            ts_parser
            (ConstPtr nullPtr)
            (ConstPtr string)
            (fromIntegral string_length)
        nullPtr @=? tree_p

-- | Internal helper: Cast a t`ConstPtr` using `castPtr`.
castConstPtr :: ConstPtr a -> ConstPtr b
castConstPtr = ConstPtr . castPtr . unConstPtr
