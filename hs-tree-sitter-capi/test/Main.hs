-- | Test suite for @tree-sitter-capi@.
module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.TreeSitter.CApi qualified as CApi

-- | All tests for @tree-sitter-capi@.
main :: IO ()
main = do
  defaultMain $
    testGroup "TreeSitter" [CApi.tests]
