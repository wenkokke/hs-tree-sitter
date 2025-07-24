{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Test.Tasty (askOption, defaultMain, localOption, testGroup)
import Test.Tasty.Patterns.Types (Expr (..))
import Test.Tasty.Runners (TestPattern (..))
import Test.TreeSitter qualified as TreeSitter
import Test.TreeSitter.Corpus (makeCorpusTests)
import TreeSitter.JavaScript qualified as JavaScript
import TreeSitter.While qualified as While

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Avoid partial function" :: String) #-}

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Generate JavaScript corpus tests
  javascriptCorpusTests <-
    JavaScript.getTestCorpusDir
      >>= makeCorpusTests JavaScript.tree_sitter_javascript
      <&> testGroup "Corpus"
  -- TODO: These failing tests are the result of the corpus parser,
  --       which is not being respectful and should be parsing the
  --       contents of the corpus files as a ByteString.
  let ignoreKnownFailures =
        foldr1
          And
          [ Not (ERE "Non-breaking spaces as whitespace")
          , Not (ERE "U+2028 as a line terminator")
          , Not (ERE "Unicode identifiers")
          ]
  -- Generate WHILE corpus tests
  whileCorpusTests <-
    While.getTestCorpusDir
      >>= makeCorpusTests While.tree_sitter_while
      <&> testGroup "Corpus"
  -- Compose test suite
  defaultMain $
    testGroup
      "TreeSitter"
      [ TreeSitter.tests
      , askOption $ \(TestPattern maybeExpr) -> do
          let testPattern =
                TestPattern . Just $
                  maybe ignoreKnownFailures (ignoreKnownFailures `And`) maybeExpr
          localOption testPattern javascriptCorpusTests
      , whileCorpusTests
      ]
