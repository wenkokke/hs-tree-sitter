{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.TreeSitter where

import Control.Exception (handle)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (for_)
import Data.GraphViz.Commands (GraphvizOutput (..), runGraphviz)
import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.IORef qualified as IORef
import Data.Maybe (isJust)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Paths_hs_tree_sitter_test (getDataFileName)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC, performMinorGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TreeSitter qualified as TS
import TreeSitter.JavaScript (tree_sitter_javascript)
import TreeSitter.While (tree_sitter_while)

tests :: TestTree
tests =
  testGroup
    "Internal"
    [ test_parserSetLogger
    , test_treePrintDotGraph
    , test_queryErrorNew
    , test_parserParse
    , test_parseJQuery
    ]

-- | Does query error conversion work?
test_queryErrorNew :: TestTree
test_queryErrorNew =
  testCase "test_queryErrorNew" $ do
    -- Create the language
    languageWhile <- TS.unsafeToLanguage =<< tree_sitter_while
    -- Create the query
    handle queryErrorHandler $ do
      query <- TS.queryNew languageWhile (BSC.pack "(thisGrammarTypeDoesNotExist)")
      TS.unsafeQueryDelete query
 where
  queryErrorHandler :: TS.QueryError -> IO ()
  queryErrorHandler queryError =
    assertEqual "query error was not QueryErrorNodeType" TS.QueryErrorTypeNodeType (TS.queryErrorType queryError)

-- | Does rendering to DOT files work?
test_treePrintDotGraph :: TestTree
test_treePrintDotGraph =
  testCase "test_treePrintDotGraph" $ do
    -- Create the parser
    parser <- TS.parserNew
    -- Set the language
    languageWhile <- TS.unsafeToLanguage =<< tree_sitter_while
    success <- TS.parserSetLanguage parser languageWhile
    unless success (assertFailure "failed to set parser language")
    -- An example program
    let program =
          unlines
            [ "x := 0;"
            , "y := x + 1"
            ]
    -- Parse example program
    maybeTree1 <- TS.parserParseString parser Nothing program
    tree1 <- maybe (assertFailure "failed to parse the program") pure maybeTree1
    withSystemTempDirectory "tree_sitter_test_treePrintDotGraph" $ \tempDir -> do
      let treeDotFile1 = tempDir </> "tree1.dot"
      withFile treeDotFile1 WriteMode $ \treeDotHandle1 ->
        TS.treePrintDotGraph tree1 treeDotHandle1
      treeDotFileExists1 <- doesFileExist treeDotFile1
      assertBool "dot file exists" treeDotFileExists1
      treeDotFileContent1 <- TL.readFile treeDotFile1
      let treeDotGraph1 = parseDotGraph @DotGraph @TL.Text treeDotFileContent1
      let treePngFile1 = tempDir </> "tree1.png"
      treePngFile1' <- runGraphviz treeDotGraph1 Png treePngFile1
      assertEqual "graphviz changed output file path" treePngFile1 treePngFile1'

-- | Does the logger work?
test_parserSetLogger :: TestTree
test_parserSetLogger =
  testCase "test_parserSetLogger" $ do
    -- Create the parser
    parser <- TS.parserNew
    -- Set the language
    languageWhile <- TS.unsafeToLanguage =<< tree_sitter_while
    success <- TS.parserSetLanguage parser languageWhile
    unless success (assertFailure "failed to set parser language")
    -- Assert the parser has no logger
    hasLogger <- TS.parserHasLogger parser
    assertBool "parser has logger" (not hasLogger)
    -- Set the logger
    let logState1 = []
    logStateRef <- IORef.newIORef logState1
    let logFun _logType msg =
          IORef.atomicModifyIORef' logStateRef $ \logState ->
            (BSC.unpack msg : logState, ())
    TS.parserSetLogger parser logFun
    -- An example program
    let program =
          unlines
            [ "x := 0;"
            , "y := x + 1"
            ]
    -- Parse example program (with logger)
    maybeTree1 <- TS.parserParseString parser Nothing program
    tree1 <- maybe (assertFailure "failed to parse the program") pure maybeTree1
    rootNode1 <- TS.treeRootNode tree1
    rootNodeString1 <- TS.showNodeAsString rootNode1
    assertBool "rootNode string is empty" (not . null $ rootNodeString1)
    -- Check logger state (should NOT be empty)
    logState2 <- IORef.readIORef logStateRef
    IORef.writeIORef logStateRef []
    assertBool "parse log is empty" (not . null $ logState2)
    -- Remove logger
    maybeLogFun <- TS.parserRemoveLogger parser
    assertBool "removed logger is NULL" (isJust maybeLogFun)
    -- Parse example program (without logger)
    maybeTree2 <- TS.parserParseString parser Nothing program
    tree2 <- maybe (assertFailure "failed to parse the program") pure maybeTree2
    rootNode2 <- TS.treeRootNode tree2
    rootNodeString2 <- TS.showNodeAsString rootNode2
    assertBool "rootNode string is empty" (not . null $ rootNodeString2)
    -- Check logger state (should be empty)
    logState3 <- IORef.readIORef logStateRef
    assertBool "parse log is not empty" (null logState3)
    TS.unsafeParserDelete parser
    TS.unsafeLanguageDelete languageWhile
    pure ()

-- | Does the parser with callback work?
test_parserParse :: TestTree
test_parserParse =
  testCase "test_parserParse" $ do
    -- Create the parser
    parser <- TS.parserNew
    -- Set the language
    languageWhile <- TS.unsafeToLanguage =<< tree_sitter_while
    success <- TS.parserSetLanguage parser languageWhile
    unless success (assertFailure "failed to set parser language")
    -- An example program
    let program :: ByteString
        program =
          BSC.unlines
            [ "x := 0;"
            , "y := x + 1"
            ]
    let input :: TS.Input
        input byteIndex _position = do
          let start = fromIntegral byteIndex - 1
          pure $ BS.take 2 (BS.drop start program)
    maybeTree <- TS.parserParse parser Nothing input TS.InputEncodingUTF8
    tree <- maybe (assertFailure "failed to parse the program") pure maybeTree
    rootNode <- TS.treeRootNode tree
    rootNodeString <- TS.showNodeAsString rootNode
    assertBool "rootNode string is empty" (not . null $ rootNodeString)

-- | Does tree-sitter-javascript parse jQuery?
test_parseJQuery :: TestTree
test_parseJQuery = do
  testCase "test_parseJQuery" $ do
    -- Create the parser
    parser <- TS.parserNew
    -- Set the language
    languageJavaScript <- TS.unsafeToLanguage =<< tree_sitter_javascript
    success <- TS.parserSetLanguage parser languageJavaScript
    unless success (assertFailure "failed to set parser language")
    -- Get the jQuery source file
    jQueryFile <- getDataFileName "test/data/jQuery.js"
    jQueryContent <- BS.readFile jQueryFile
    let input :: TS.Input
        input byteIndex _position = do
          performMinorGC
          let start = fromIntegral byteIndex
          let chunk = BS.take 4096 (BS.drop (start - 1) jQueryContent)
          -- NOTE: copy the chunk to ensure it has its own memory (to leak)
          pure $ BS.copy chunk
    for_ [1 .. 1000] $ \(_time :: Int) -> do
      maybeTree <- TS.parserParse parser Nothing input TS.InputEncodingUTF8
      tree <- maybe (assertFailure "failed to parse the program") pure maybeTree
      rootNode <- TS.treeRootNode tree
      rootNodeString <- TS.showNodeAsString rootNode
      assertBool "rootNode string is empty" (not . null $ rootNodeString)
      performMajorGC
