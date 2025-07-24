{-# LANGUAGE CApiFFI #-}

module TreeSitter.While (
  tree_sitter_while,
  getNodeTypesPath,
  getTestCorpusDir,
) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Paths_tree_sitter_while (getDataFileName)

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "test/corpus"

data
  {-# CTYPE "tree-sitter-while.h" "TSLanguage" #-}
  TSLanguage

foreign import capi unsafe "tree-sitter-while.h tree_sitter_while"
  tree_sitter_while ::
    IO (ConstPtr TSLanguage)
