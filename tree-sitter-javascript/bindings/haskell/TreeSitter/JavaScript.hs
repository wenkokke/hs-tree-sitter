{-# LANGUAGE CApiFFI #-}

module TreeSitter.JavaScript (
  tree_sitter_javascript,
  getNodeTypesPath,
  getTestCorpusDir,
) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Paths_tree_sitter_javascript (getDataFileName)

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-javascript/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-javascript/test/corpus"

data
  {-# CTYPE "tree-sitter-javascript.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree-sitter-javascript.h tree_sitter_javascript"
  tree_sitter_javascript ::
    IO (ConstPtr TSLanguage)
