{-# LANGUAGE OverloadedStrings #-}

module Test.TreeSitter.Corpus where

import Control.Monad (forM, unless)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Foreign.C.ConstPtr.Compat (ConstPtr)
import System.FilePath (makeRelative)
import System.FilePath.Glob qualified as Glob (globDir1)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertFailure,
  testCase,
 )
import TreeSitter qualified as TS
import TreeSitter.Corpus qualified as TSC
import TreeSitter.SExp (
  loose,
  parseSExp,
  prettyParseError,
  prettySExp,
  prettySExpDiff,
 )

-- TODO: makeCorpusTest should read the file as a ByteString in order
--       to preserve whatever encoding is present
-- TODO: makeCorpusTest should store the position in the file, rather
--       than the entire string, in order to lower memory consumption
--       in the test suite

makeCorpusTests :: IO (ConstPtr tsLanguage) -> FilePath -> IO [TestTree]
makeCorpusTests languageConstructor corpusDirectory = do
  corpusFiles <- Glob.globDir1 "**/*.txt" corpusDirectory
  forM corpusFiles $ \corpusFile -> do
    let testName = makeRelative corpusDirectory corpusFile
    mentries <- TSC.readCorpusFile corpusFile
    pure $
      case mentries of
        Left errorMessage ->
          testCase testName (assertFailure errorMessage)
        Right entries ->
          testGroup testName (makeCorpusTest languageConstructor corpusFile <$> entries)

makeCorpusTest :: IO (ConstPtr tsLanguage) -> FilePath -> TSC.TestCase -> TestTree
makeCorpusTest languageConstructor corpusFile entry =
  testCase (TSC.nameString entry) $ do
    parser <- TS.parserNew
    language <- TS.unsafeToLanguage =<< languageConstructor
    success <- TS.parserSetLanguage parser language
    assertBool "parserSetLanguage: failed" success
    mtree <- TS.parserParseString parser Nothing $ TSC.codeString entry
    case mtree of
      Nothing -> assertFailure "parserParseString: failed"
      Just tree -> do
        node <- TS.treeRootNode tree
        sexpByteString <- TS.showNode node
        expectSExp <-
          either (assertFailure . prettyParseError) (pure . loose) $
            parseSExp corpusFile (TL.toStrict (TSC.sexp entry))
        actualSExp <-
          either (assertFailure . prettyParseError) (pure . loose) $
            parseSExp "tree-sitter" (TE.decodeUtf8 sexpByteString)
        unless (expectSExp == actualSExp) . assertFailure . unlines $
          [ "Found S-expression that was different from the expected result."
          , ""
          , "Source:"
          , TSC.codeString entry
          , ""
          , "Result Diff:"
          , prettySExpDiff expectSExp actualSExp
          , ""
          , "Expected Result:"
          , prettySExp expectSExp
          , ""
          , "Actual Result:"
          , prettySExp actualSExp
          ]
