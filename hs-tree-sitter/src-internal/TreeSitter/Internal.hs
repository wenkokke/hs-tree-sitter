{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TreeSitter.Internal (
  -- * ABI Versioning
  C.TREE_SITTER_LANGUAGE_VERSION,
  C.TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION,

  -- * Types
  Symbol (..),
  StateId (..),
  GrammarType (..),
  FieldId (..),
  FieldName (..),
  CaptureName (..),
  CaptureIndex (..),
  PatternIndex (..),
  Language,
  Parser,
  Tree,
  Query,
  QueryCursor,
  LookaheadIterator,
  InputEncoding (InputEncodingUTF8, InputEncodingUTF16),
  SymbolType (SymbolTypeRegular, SymbolTypeAnonymous, SymbolTypeSupertype, SymbolTypeAuxiliary),
  Point (Point, pointColumn, pointRow),
  Range (Range, rangeStartPoint, rangeEndPoint, rangeStartByte, rangeEndByte),
  Input,
  LogType (LogTypeLex, LogTypeParse),
  InputEdit (InputEdit, inputEditStartByte, inputEditOldEndByte, inputEditNewEndByte, inputEditStartPoint, inputEditOldEndPoint, inputEditNewEndPoint),
  Node,
  NodeId (..),
  nodeId,
  TreeCursor,
  TreeCursorId (..),
  QueryCapture,
  Quantifier,
  QueryMatch,
  QueryPredicateStepType,
  QueryPredicateStep,
  QueryErrorType (QueryErrorTypeSyntax, QueryErrorTypeNodeType, QueryErrorTypeField, QueryErrorTypeCapture, QueryErrorTypeStructure, QueryErrorTypeLanguage),
  QueryError (..),

  -- * Parser
  parserNew,
  unsafeParserDelete,
  withParser,
  parserSetLanguage,
  parserLanguage,
  parserSetIncludedRanges,
  parserIncludedRanges,
  parserSetLogger,
  parserLogger,
  parserHasLogger,
  parserRemoveLogger,
  parserParse,
  parserParseString,
  parserParseByteString,
  parserParseByteStringWithEncoding,
  parserReset,
  Microsecond (..),
  parserSetTimeoutMicros,
  parserTimeoutMicros,
  CancellationFlag (Cancel, Continue),
  CancellationFlagRef,
  getCancellationFlag,
  putCancellationFlag,
  parserSetCancellationFlag,
  parserCancellationFlag,
  parserPrintDotGraphs,

  -- * Tree
  treeCopy,
  unsafeTreeDelete,
  treeRootNode,
  treeRootNodeWithOffset,
  treeLanguage,
  treeIncludedRanges,
  treeEdit,
  treeGetChangedRanges,
  treePrintDotGraph,

  -- * Node
  nodeType,
  nodeTypeAsString,
  nodeSymbol,
  nodeLanguage,
  nodeGrammarType,
  nodeGrammarTypeAsString,
  nodeGrammarSymbol,
  nodeRange,
  nodeStartByte,
  nodeStartPoint,
  nodeEndByte,
  nodeEndPoint,
  showNode,
  showNodeAsString,
  nodeIsNull,
  nodeIsNamed,
  nodeIsMissing,
  nodeIsExtra,
  nodeHasChanges,
  nodeHasError,
  nodeIsError,
  nodeParseState,
  nodeNextParseState,
  nodeParent,
  nodeChildWithDescendant,
  nodeChild,
  nodeFieldNameForChild,
  nodeFieldNameForChildAsString,
  nodeFieldNameForNamedChild,
  nodeFieldNameForNamedChildAsString,
  nodeChildCount,
  nodeNamedChild,
  nodeNamedChildCount,
  nodeChildByFieldName,
  nodeChildByFieldId,
  nodeNextSibling,
  nodePrevSibling,
  nodeNextNamedSibling,
  nodePrevNamedSibling,
  nodeFirstChildForByte,
  nodeFirstNamedChildForByte,
  nodeDescendantCount,
  nodeDescendantForByteRange,
  nodeDescendantForPointRange,
  nodeNamedDescendantForByteRange,
  nodeNamedDescendantForPointRange,
  nodeEdit,
  nodeEq,

  -- * TreeCursor
  treeCursorNew,
  unsafeTreeCursorDelete,
  treeCursorReset,
  treeCursorResetTo,
  treeCursorCurrentNode,
  treeCursorCurrentFieldName,
  treeCursorCurrentFieldId,
  treeCursorGotoParent,
  treeCursorGotoNextSibling,
  treeCursorGotoPreviousSibling,
  treeCursorGotoFirstChild,
  treeCursorGotoLastChild,
  treeCursorGotoDescendant,
  treeCursorCurrentDescendantIndex,
  treeCursorCurrentDepth,
  treeCursorGotoFirstChildForByte,
  treeCursorGotoFirstChildForPoint,
  treeCursorCopy,

  -- * Query
  queryNew,
  unsafeQueryDelete,
  queryPatternCount,
  queryCaptureCount,
  queryStringCount,
  queryStartByteForPattern,
  queryEndByteForPattern,
  queryPredicatesForPattern,
  queryIsPatternRooted,
  queryIsPatternNonLocal,
  queryIsPatternGuaranteedAtStep,
  queryCaptureNameForIndex,
  queryCaptureQuantifierForIndex,
  queryStringValueForIndex,
  queryDisableCapture,
  queryDisablePattern,
  queryCursorNew,
  unsafeQueryCursorDelete,
  queryCursorExec,
  queryCursorDidExceedMatchLimit,
  queryCursorMatchLimit,
  queryCursorSetMatchLimit,
  queryCursorSetTimeoutMicros,
  queryCursorTimeoutMicros,
  queryCursorSetByteRange,
  queryCursorSetPointRange,
  queryCursorNextMatch,
  queryCursorRemoveMatch,
  queryCursorNextCapture,
  queryCursorSetMaxStartDepth,

  -- * Language
  unsafeToLanguage,
  unsafeLanguageDelete,
  languageCopy,
  languageSymbolCount,
  languageStateCount,
  languageSymbolName,
  languageSymbolForGrammarType,
  languageFieldCount,
  languageFieldNameForId,
  languageFieldIdForName,
  languageSymbolType,
  languageVersion,
  languageNextState,

  -- * Lookahead Iterator
  withLookaheadIteratorAsTSLookaheadIteratorPtr,
  lookaheadIteratorNew,
  unsafeLookaheadIteratorDelete,
  lookaheadIteratorResetState,
  lookaheadIteratorReset,
  lookaheadIteratorLanguage,
  lookaheadIteratorNext,
  lookaheadIteratorCurrentSymbol,
  lookaheadIteratorCurrentSymbolName,

  -- * Global Configuration
) where

import Control.Exception (Exception (..), assert, bracket, throwIO)
import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Internal (ByteString (BS))
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce (coerce)
import Data.IORef (newIORef, writeIORef)
import Data.Maybe (isJust)
import Foreign
import Foreign.C (CBool, CInt (CInt), CSize (..))
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.IO.FD (FD (..))
import GHC.IO.Handle.FD (handleToFd)
import System.IO (Handle)
import TreeSitter.CApi qualified as C
import TreeSitter.CApi qualified as TSNode (TSNode (..))

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}

{-# ANN module ("HLint: ignore Foreign should be imported post-qualified or with an explicit import list" :: String) #-}

--------------------------------------------------------------------------------

-- * Types

newtype CaptureIndex = WrapTSCaptureIndex {unWrapTSCaptureIndex :: Word32}

newtype PatternIndex = WrapTSPatternIndex {unWrapTSPatternIndex :: Word32}

newtype StateId = WrapTSStateId {unWrapTSStateId :: C.TSStateId}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

newtype Symbol = WrapTSSymbol {unWrapTSSymbol :: C.TSSymbol}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

newtype GrammarType = WrapTSGrammarType {unWrapTSGrammarType :: ByteString}
  deriving stock (Show, Read, Eq, Ord)

newtype FieldId = WrapTSFieldId {unWrapTSFieldId :: C.TSFieldId}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

newtype FieldName = WrapTSFieldName {unWrapTSFieldName :: ByteString}
  deriving stock (Show, Read, Eq, Ord)

newtype CaptureName = WrapTSCaptureName {unWrapTSCaptureName :: ByteString}
  deriving stock (Show, Read, Eq, Ord)

newtype Language = WrapTSLanguage {unWrapTSLanguage :: ForeignPtr C.TSLanguage}

newtype Parser = WrapTSParser {unWrapTSParser :: ForeignPtr C.TSParser}

-- | __Warning__: 'Tree' is not thread-safe. To use a 'Tree' from multiple threads, you must copy it using 'treeCopy'.
newtype Tree = WrapTSTree {unWrapTSTree :: ForeignPtr C.TSTree}

newtype Query = WrapTSQuery {unWrapTSQuery :: ForeignPtr C.TSQuery}

newtype QueryCursor = WrapTSQueryCursor {unWrapTSQueryCursor :: ForeignPtr C.TSQueryCursor}

newtype LookaheadIterator = WrapTSLookaheadIterator {unWrapTSLookaheadIterator :: ForeignPtr C.TSLookaheadIterator}

newtype InputEncoding = WrapTSInputEncoding {unWrapTSInputEncoding :: C.TSInputEncoding}

pattern InputEncodingUTF8 :: InputEncoding
pattern InputEncodingUTF8 = WrapTSInputEncoding C.TSInputEncodingUTF8

pattern InputEncodingUTF16 :: InputEncoding
pattern InputEncodingUTF16 = WrapTSInputEncoding C.TSInputEncodingUTF16

{-# COMPLETE InputEncodingUTF8, InputEncodingUTF16 #-}

instance Show InputEncoding where
  show InputEncodingUTF8 = "InputEncodingUTF8"
  show InputEncodingUTF16 = "InputEncodingUTF16"

newtype SymbolType = WrapTSSymbolType {unWrapTSSymbolType :: C.TSSymbolType}
  deriving stock (Eq)

pattern SymbolTypeRegular :: SymbolType
pattern SymbolTypeRegular = WrapTSSymbolType C.TSSymbolTypeRegular

pattern SymbolTypeAnonymous :: SymbolType
pattern SymbolTypeAnonymous = WrapTSSymbolType C.TSSymbolTypeAnonymous

pattern SymbolTypeSupertype :: SymbolType
pattern SymbolTypeSupertype = WrapTSSymbolType C.TSSymbolTypeSupertype

pattern SymbolTypeAuxiliary :: SymbolType
pattern SymbolTypeAuxiliary = WrapTSSymbolType C.TSSymbolTypeAuxiliary

{-# COMPLETE SymbolTypeRegular, SymbolTypeAnonymous, SymbolTypeSupertype, SymbolTypeAuxiliary #-}

instance Show SymbolType where
  show SymbolTypeRegular = "SymbolTypeRegular"
  show SymbolTypeAnonymous = "SymbolTypeAnonymous"
  show SymbolTypeSupertype = "SymbolTypeSupertype"
  show SymbolTypeAuxiliary = "SymbolTypeAuxiliary"

newtype Point = WrapTSPoint {unWrapTSPoint :: C.TSPoint}
  deriving newtype (Ord, Eq)

pattern Point :: Word32 -> Word32 -> Point
pattern Point
  { pointRow
  , pointColumn
  } =
  WrapTSPoint
    ( C.TSPoint
        pointRow
        pointColumn
      )

{-# COMPLETE Point #-}

instance Show Point where
  showsPrec :: Int -> Point -> ShowS
  showsPrec p Point{..} =
    showParen (p > 10) $
      showString "Point "
        . showsPrec 11 pointRow
        . showChar ' '
        . showsPrec 11 pointColumn

newtype Range = WrapTSRange {unWrapTSRange :: C.TSRange}
  deriving stock (Eq)

pattern Range :: Point -> Point -> Word32 -> Word32 -> Range
pattern Range
  { rangeStartPoint
  , rangeEndPoint
  , rangeStartByte
  , rangeEndByte
  } <-
  WrapTSRange
    ( C.TSRange
        (WrapTSPoint -> rangeStartPoint)
        (WrapTSPoint -> rangeEndPoint)
        rangeStartByte
        rangeEndByte
      )
  where
    Range startPoint endPoint startByte endByte =
      WrapTSRange
        ( C.TSRange
            (unWrapTSPoint startPoint)
            (unWrapTSPoint endPoint)
            startByte
            endByte
        )

{-# COMPLETE Range #-}

instance Show Range where
  showsPrec :: Int -> Range -> ShowS
  showsPrec p Range{..} =
    showParen (p > 10) $
      showString "Range "
        . showsPrec 11 rangeStartPoint
        . showChar ' '
        . showsPrec 11 rangeEndPoint
        . showChar ' '
        . showsPrec 11 rangeStartByte
        . showChar ' '
        . showsPrec 11 rangeEndByte

newtype LogType = WrapTSLogType {unWrapTSLogType :: C.TSLogType}
  deriving (Eq)

pattern LogTypeParse :: LogType
pattern LogTypeParse = WrapTSLogType C.TSLogTypeParse

pattern LogTypeLex :: LogType
pattern LogTypeLex = WrapTSLogType C.TSLogTypeLex

{-# COMPLETE LogTypeParse, LogTypeLex #-}

instance Show LogType where
  show LogTypeParse = "LogTypeParse"
  show LogTypeLex = "LogTypeLex"

type Log = LogType -> ByteString -> IO ()

logToTSLog :: Log -> C.TSLog
logToTSLog logFun = \logType logMsg ->
  BS.packCString (unConstPtr logMsg) >>= logFun (coerce logType)

tsLogToLog :: C.TSLog -> Log
tsLogToLog logFun = \logType logMsg ->
  BS.useAsCString logMsg $ logFun (coerce logType) . ConstPtr

newtype InputEdit = WrapTSInputEdit {unWrapTSInputEdit :: C.TSInputEdit}

pattern InputEdit :: Word32 -> Word32 -> Word32 -> Point -> Point -> Point -> InputEdit
pattern InputEdit
  { inputEditStartByte
  , inputEditOldEndByte
  , inputEditNewEndByte
  , inputEditStartPoint
  , inputEditOldEndPoint
  , inputEditNewEndPoint
  } <-
  WrapTSInputEdit
    ( C.TSInputEdit
        inputEditStartByte
        inputEditOldEndByte
        inputEditNewEndByte
        (WrapTSPoint -> inputEditStartPoint)
        (WrapTSPoint -> inputEditOldEndPoint)
        (WrapTSPoint -> inputEditNewEndPoint)
      )
  where
    InputEdit startByte oldEndByte newEndByte startPoint oldEndPoint newEndPoint =
      WrapTSInputEdit
        ( C.TSInputEdit
            startByte
            oldEndByte
            newEndByte
            (unWrapTSPoint startPoint)
            (unWrapTSPoint oldEndPoint)
            (unWrapTSPoint newEndPoint)
        )

newtype Node = WrapTSNode {unWrapTSNode :: C.TSNode}

newtype NodeId = WrapTSNodeId {unWrapTSNodeId :: Int}
  deriving stock (Eq, Ord)
  deriving newtype (Show)

nodeId :: Node -> NodeId
nodeId = coerce . ptrToIntPtr . coerce . TSNode._id . coerce

newtype TreeCursor = WrapTSTreeCursor {unWrapTSTreeCursor :: ForeignPtr C.TSTreeCursor}

newtype TreeCursorId = WrapTSTreeCursorId {unWrapTSTreeCursorId :: Word}
  deriving (Eq, Ord)

-- treeCursorId :: TreeCursor -> TreeCursorId
-- treeCursorId = coerce . ptrToWordPtr . coerce . TSTreeCursor._id . coerce

newtype QueryCapture = WrapTSQueryCapture {unWrapTSQueryCapture :: C.TSQueryCapture}

newtype Quantifier = WrapTSQuantifier {unWrapTSQuantifier :: C.TSQuantifier}
  deriving stock (Eq)

pattern QuantifierZero :: Quantifier
pattern QuantifierZero = WrapTSQuantifier C.TSQuantifierZero

pattern QuantifierZeroOrOne :: Quantifier
pattern QuantifierZeroOrOne = WrapTSQuantifier C.TSQuantifierZeroOrOne

pattern QuantifierZeroOrMore :: Quantifier
pattern QuantifierZeroOrMore = WrapTSQuantifier C.TSQuantifierZeroOrMore

pattern QuantifierOne :: Quantifier
pattern QuantifierOne = WrapTSQuantifier C.TSQuantifierOne

pattern QuantifierOneOrMore :: Quantifier
pattern QuantifierOneOrMore = WrapTSQuantifier C.TSQuantifierOneOrMore

{-# COMPLETE QuantifierZero, QuantifierZeroOrOne, QuantifierZeroOrMore, QuantifierOne, QuantifierOneOrMore #-}

instance Show Quantifier where
  show QuantifierZero = "QuantifierZero"
  show QuantifierZeroOrOne = "QuantifierZeroOrOne"
  show QuantifierZeroOrMore = "QuantifierZeroOrMore"
  show QuantifierOne = "QuantifierOne"
  show QuantifierOneOrMore = "QuantifierOneOrMore"

newtype QueryMatch = WrapTSQueryMatch {unWrapTSQueryMatch :: C.TSQueryMatch}

newtype QueryPredicateStepType = WrapTSQueryPredicateStepType {unWrapTSQueryPredicateStepType :: C.TSQueryPredicateStepType}
  deriving stock (Eq)

pattern QueryPredicateStepTypeDone :: QueryPredicateStepType
pattern QueryPredicateStepTypeDone = WrapTSQueryPredicateStepType C.TSQueryPredicateStepTypeDone

pattern QueryPredicateStepTypeCapture :: QueryPredicateStepType
pattern QueryPredicateStepTypeCapture = WrapTSQueryPredicateStepType C.TSQueryPredicateStepTypeCapture

pattern QueryPredicateStepTypeString :: QueryPredicateStepType
pattern QueryPredicateStepTypeString = WrapTSQueryPredicateStepType C.TSQueryPredicateStepTypeString

{-# COMPLETE QueryPredicateStepTypeDone, QueryPredicateStepTypeCapture, QueryPredicateStepTypeString #-}

instance Show QueryPredicateStepType where
  show QueryPredicateStepTypeDone = "QueryPredicateStepTypeDone"
  show QueryPredicateStepTypeCapture = "QueryPredicateStepTypeCapture"
  show QueryPredicateStepTypeString = "QueryPredicateStepTypeString"

newtype QueryPredicateStep = WrapTSQueryPredicateStep {unWrapTSQueryPredicateStep :: C.TSQueryPredicateStep}

newtype QueryErrorType = WrapTSQueryErrorType {unWrapTSQueryErrorType :: C.TSQueryError}
  deriving stock (Eq)

pattern QueryErrorTypeSyntax :: QueryErrorType
pattern QueryErrorTypeSyntax = WrapTSQueryErrorType C.TSQueryErrorSyntax

pattern QueryErrorTypeNodeType :: QueryErrorType
pattern QueryErrorTypeNodeType = WrapTSQueryErrorType C.TSQueryErrorNodeType

pattern QueryErrorTypeField :: QueryErrorType
pattern QueryErrorTypeField = WrapTSQueryErrorType C.TSQueryErrorField

pattern QueryErrorTypeCapture :: QueryErrorType
pattern QueryErrorTypeCapture = WrapTSQueryErrorType C.TSQueryErrorCapture

pattern QueryErrorTypeStructure :: QueryErrorType
pattern QueryErrorTypeStructure = WrapTSQueryErrorType C.TSQueryErrorStructure

pattern QueryErrorTypeLanguage :: QueryErrorType
pattern QueryErrorTypeLanguage = WrapTSQueryErrorType C.TSQueryErrorLanguage

{-# COMPLETE QueryErrorTypeSyntax, QueryErrorTypeNodeType, QueryErrorTypeField, QueryErrorTypeCapture, QueryErrorTypeStructure, QueryErrorTypeLanguage #-}

instance Show QueryErrorType where
  show QueryErrorTypeSyntax = "QueryErrorTypeSyntax"
  show QueryErrorTypeNodeType = "QueryErrorTypeNodeType"
  show QueryErrorTypeField = "QueryErrorTypeField"
  show QueryErrorTypeCapture = "QueryErrorTypeCapture"
  show QueryErrorTypeStructure = "QueryErrorTypeStructure"
  show QueryErrorTypeLanguage = "QueryErrorTypeLanguage"

data QueryError = QueryError
  { queryErrorOffset :: !Word32
  , queryErrorPoint :: !Point
  , queryErrorMessage :: !String
  , queryErrorType :: !QueryErrorType
  }
  deriving (Show)

instance Exception QueryError where
  displayException :: QueryError -> String
  displayException = queryErrorMessage

-- * Parser

withParserAsTSParserPtr :: Parser -> (Ptr C.TSParser -> IO a) -> IO a
withParserAsTSParserPtr = withForeignPtr . coerce

-- | See @`C.ts_parser_new`@.
parserNew :: IO Parser
parserNew = coerce newForeignPtr C.p_ts_parser_delete =<< C.ts_parser_new

{-| Delete the 'Parser' and free the associated memory.

See @`C.ts_parser_delete`@.

__Warning__: Using the 'Parser' after calling 'unsafeParserDelete' leads to undefined behaviour.

__Warning__: If the 'Parser' has a logger, this function does not delete it.
-}
unsafeParserDelete :: Parser -> IO ()
unsafeParserDelete = coerce finalizeForeignPtr

withParser :: (Parser -> IO a) -> IO a
withParser = bracket parserNew unsafeParserDelete

-- | See @`C.ts_parser_language`@.
parserLanguage :: Parser -> IO Language
parserLanguage parser =
  withParserAsTSParserPtr parser $
    unsafeToLanguage <=< coerce C.ts_parser_language

-- | See @`C.ts_parser_set_language`@.
parserSetLanguage :: Parser -> Language -> IO Bool
parserSetLanguage parser language =
  withParserAsTSParserPtr parser $ \parserPtr -> do
    withLanguageAsTSLanguagePtr language $ \languagePtr -> do
      success <- C.ts_parser_set_language parserPtr languagePtr
      pure $ toBool success

-- | See @`C.ts_parser_set_included_ranges`@.
parserSetIncludedRanges :: Parser -> [Range] -> IO Bool
parserSetIncludedRanges parser ranges =
  withParserAsTSParserPtr parser $ \parserPtr -> do
    let rangesLength = length ranges
    withArray (coerce ranges) $ \rangesPtr -> do
      success <-
        C.ts_parser_set_included_ranges
          parserPtr
          (ConstPtr rangesPtr)
          (fromIntegral rangesLength)
      pure $ toBool success

-- | See @`C.ts_parser_included_ranges`@.
parserIncludedRanges :: Parser -> IO [Range]
parserIncludedRanges parser =
  withParserAsTSParserPtr parser $ \parserPtr -> do
    alloca $ \rangesLengthPtr -> do
      rangesPtr <-
        C.ts_parser_included_ranges
          (ConstPtr parserPtr)
          rangesLengthPtr
      rangesLength <- fromIntegral <$> peek rangesLengthPtr
      coerce $ peekArray rangesLength (unConstPtr rangesPtr)

{-| Set the 'Logger' for a 'Parser'.

  See @`C.ts_parser_set_logger`@.
-}
parserSetLogger :: Parser -> Log -> IO ()
parserSetLogger parser logFun =
  withParserAsTSParserPtr parser $ \parserPtr ->
    C.ts_parser_set_logger (coerce parserPtr) (logToTSLog logFun)

{-| Get the 'Logger' for a 'Parser'.

  If no 'Logger' was set, 'parserLogger' returns 'Nothing'.

  See @`C.ts_parser_logger`@.
-}
parserLogger :: Parser -> IO (Maybe Log)
parserLogger parser =
  withParserAsTSParserPtr parser $
    fmap (fmap tsLogToLog) . C.ts_parser_logger . coerce

-- | Test if the 'Parser' has a 'Logger'.
parserHasLogger :: Parser -> IO Bool
parserHasLogger = fmap isJust . parserLogger

-- | If the 'Parser' has a 'Logger', remove and pure it.
parserRemoveLogger :: Parser -> IO (Maybe Log)
parserRemoveLogger parser =
  withParserAsTSParserPtr parser $
    fmap (fmap tsLogToLog) . C.ts_parser_remove_logger . coerce

type Input =
  -- | Byte index.
  Word32 ->
  -- | Position.
  Point ->
  IO ByteString

-- | See @`C.ts_parser_parse`@.
parserParse :: Parser -> Maybe Tree -> Input -> InputEncoding -> IO (Maybe Tree)
parserParse parser oldTree input encoding =
  withParserAsTSParserPtr parser $ \parserPtr ->
    withMaybeTreeAsTSTreePtr oldTree $ \oldTreePtr -> do
      -- NOTE: The purpose of `chunkRef` is to hold on to a reference to
      --       the current chunk and prevent its garbage collection until
      --       the next call to `tsRead`.
      --       Incorrect management of these references can potentially cause
      --       either a memory leak or a use-after-free error, since by using
      --       `unsafeForeignPtrToPtr` we bypass the foreign pointer finalizer.
      -- NOTE: Despite my best efforts, I have not been able to demonstrate
      --       that this code without the `IORef` leaks memory.
      chunkRef <- newIORef BS.empty
      let tsRead = \byteIndex position_p bytesRead -> do
            position <- WrapTSPoint <$> peek position_p
            chunk@(BS chunkForeignPtr chunkLenInt) <- input byteIndex position
            writeIORef chunkRef chunk
            let chunkLen = fromIntegral chunkLenInt
            poke bytesRead chunkLen
            let chunkPtr = coerce (unsafeForeignPtrToPtr chunkForeignPtr)
            pure chunkPtr
      newTreePtr <-
        C.ts_parser_parse
          parserPtr
          (ConstPtr oldTreePtr)
          tsRead
          (coerce encoding)
      toMaybeTree newTreePtr

-- | See @`C.ts_parser_parse_string`@.
parserParseString :: Parser -> Maybe Tree -> String -> IO (Maybe Tree)
parserParseString parser oldTree string =
  parserParseByteString parser oldTree (BSC.pack string)

-- | See @`C.ts_parser_parse_string`@.
parserParseByteString :: Parser -> Maybe Tree -> ByteString -> IO (Maybe Tree)
parserParseByteString parser oldTree string =
  withParserAsTSParserPtr parser $ \parserPtr ->
    withMaybeTreeAsTSTreePtr oldTree $ \oldTreePtr ->
      BSU.unsafeUseAsCStringLen string $ \(stringPtr, stringLen) -> do
        newTreePtr <-
          C.ts_parser_parse_string
            parserPtr
            (ConstPtr oldTreePtr)
            (ConstPtr stringPtr)
            (fromIntegral stringLen)
        toMaybeTree newTreePtr

-- | See @`C.ts_parser_parse_string_encoding`@.
parserParseByteStringWithEncoding :: Parser -> Maybe Tree -> ByteString -> InputEncoding -> IO (Maybe Tree)
parserParseByteStringWithEncoding parser oldTree string inputEncoding =
  withParserAsTSParserPtr parser $ \parserPtr ->
    withMaybeTreeAsTSTreePtr oldTree $ \oldTreePtr ->
      BSU.unsafeUseAsCStringLen string $ \(stringPtr, stringLen) -> do
        newTreePtr <-
          C.ts_parser_parse_string_encoding
            parserPtr
            (ConstPtr oldTreePtr)
            (ConstPtr stringPtr)
            (fromIntegral stringLen)
            (coerce inputEncoding)
        toMaybeTree newTreePtr

-- | See @`C.ts_parser_reset`@.
parserReset :: Parser -> IO ()
parserReset = (`withParserAsTSParserPtr` C.ts_parser_reset)

-- | Microseconds.
newtype Microsecond = Microsecond {unMicrosecond :: Word64}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

-- | See @`C.ts_parser_set_timeout_micros`@.
parserSetTimeoutMicros :: Parser -> Microsecond -> IO ()
parserSetTimeoutMicros parser ms =
  withParserAsTSParserPtr parser $ \parserPtr ->
    coerce C.ts_parser_set_timeout_micros parserPtr ms

-- | See @`C.ts_parser_timeout_micros`@.
parserTimeoutMicros :: Parser -> IO Microsecond
parserTimeoutMicros parser =
  withParserAsTSParserPtr parser $
    coerce C.ts_parser_timeout_micros

newtype CancellationFlag = WrapTSCancellationFlag {unWrapTSCancellationFlag :: CSize}

isContinue :: CancellationFlag -> Bool
isContinue = (== 0) . unWrapTSCancellationFlag
{-# INLINE isContinue #-}

pattern Continue :: CancellationFlag
pattern Continue = WrapTSCancellationFlag 0

pattern Cancel :: CancellationFlag
pattern Cancel <- (isContinue -> False)
  where
    Cancel = WrapTSCancellationFlag 1

{-# COMPLETE Continue, Cancel #-}

newtype CancellationFlagRef = CancellationFlagRef {unCancellationFlagRef :: ConstPtr CSize}

putCancellationFlag :: CancellationFlagRef -> CancellationFlag -> IO ()
putCancellationFlag = coerce (poke @CSize)
{-# INLINE putCancellationFlag #-}

getCancellationFlag :: CancellationFlagRef -> IO CancellationFlag
getCancellationFlag = coerce (peek @CSize)
{-# INLINE getCancellationFlag #-}

-- | See @`C.ts_parser_set_cancellation_flag`@.
parserSetCancellationFlag :: Parser -> CancellationFlagRef -> IO ()
parserSetCancellationFlag parser cancellationFlagRef =
  withParserAsTSParserPtr parser $ \parserPtr ->
    coerce C.ts_parser_set_cancellation_flag parserPtr cancellationFlagRef

-- | See @`C.ts_parser_cancellation_flag`@.
parserCancellationFlag :: Parser -> IO CancellationFlagRef
parserCancellationFlag parser =
  withParserAsTSParserPtr parser $ \parserPtr ->
    coerce C.ts_parser_cancellation_flag parserPtr

{-| See @'C.ts_parser_print_dot_graphs'@.
| This function throws an 'IOError' if the 'Handle' does not reference a file descriptor.
-}
parserPrintDotGraphs :: Parser -> Handle -> IO ()
parserPrintDotGraphs parser handle = do
  fileDescriptor <- fdFD <$> handleToFd handle
  withParserAsTSParserPtr parser $ \parserPtr ->
    coerce C.ts_parser_print_dot_graphs parserPtr fileDescriptor

-- * Tree

toTree :: Ptr C.TSTree -> IO Tree
toTree = coerce newForeignPtr C.p_ts_tree_delete

withTreeAsTSTreePtr :: Tree -> (Ptr C.TSTree -> IO a) -> IO a
withTreeAsTSTreePtr = withForeignPtr . coerce

toMaybeTree :: Ptr C.TSTree -> IO (Maybe Tree)
toMaybeTree treePtr
  | treePtr == nullPtr = pure Nothing
  | otherwise = Just <$> toTree treePtr

withMaybeTreeAsTSTreePtr :: Maybe Tree -> (Ptr C.TSTree -> IO a) -> IO a
withMaybeTreeAsTSTreePtr mtree action =
  maybe (action nullPtr) (`withTreeAsTSTreePtr` action) mtree

-- | See @`C.ts_tree_copy`@.
treeCopy :: Tree -> IO Tree
treeCopy tree =
  withTreeAsTSTreePtr tree $
    toTree <=< C.ts_tree_copy

{-| Delete the 'Tree'.

See @`C.ts_tree_delete`@.

__Warning__: Using the 'Tree' after calling 'unsafeTreeDelete' leads to undefined behaviour.
-}
unsafeTreeDelete :: Tree -> IO ()
unsafeTreeDelete = coerce finalizeForeignPtr

-- | See @`C.ts_tree_root_node`@.
treeRootNode :: Tree -> IO Node
treeRootNode tree =
  withTreeAsTSTreePtr tree $
    coerce C.ts_tree_root_node

-- | See @`C.ts_tree_root_node_with_offset`@.
treeRootNodeWithOffset :: Tree -> Word32 -> Point -> IO Node
treeRootNodeWithOffset tree offsetBytes offsetExtent =
  withTreeAsTSTreePtr tree $ \treePtr ->
    coerce C.ts_tree_root_node_with_offset treePtr offsetBytes offsetExtent

-- | See @`C.ts_tree_language`@.
treeLanguage :: Tree -> IO Language
treeLanguage tree =
  withTreeAsTSTreePtr tree $
    unsafeToLanguage <=< C.ts_tree_language

-- | See @`C.ts_tree_included_ranges`@.
treeIncludedRanges :: Tree -> IO [Range]
treeIncludedRanges tree = do
  alloca $ \rangesLengthPtr -> do
    rangesPtr <-
      withTreeAsTSTreePtr tree $ \treePtr ->
        C.ts_tree_included_ranges treePtr rangesLengthPtr
    rangesLength <- fromIntegral <$> peek rangesLengthPtr
    coerce <$> peekArray rangesLength rangesPtr

-- | See @`C.ts_tree_edit`@.
treeEdit :: Tree -> InputEdit -> IO ()
treeEdit tree inputEdit =
  withTreeAsTSTreePtr tree $ \treePtr ->
    with (coerce inputEdit) $ \inputEditPtr ->
      C.ts_tree_edit treePtr inputEditPtr

-- | See @`C.ts_tree_get_changed_ranges`@.
treeGetChangedRanges :: Tree -> Tree -> IO [Range]
treeGetChangedRanges oldTree newTree =
  alloca $ \rangesLengthPtr -> do
    rangesPtr <-
      withTreeAsTSTreePtr oldTree $ \oldTreePtr ->
        withTreeAsTSTreePtr newTree $ \newTreePtr ->
          C.ts_tree_get_changed_ranges oldTreePtr newTreePtr rangesLengthPtr
    rangesLength <- fromIntegral <$> peek rangesLengthPtr
    coerce <$> peekArray rangesLength rangesPtr

{-| See @'C.ts_tree_print_dot_graph'@.
| This function throws an 'IOError' if the 'Handle' does not reference a file descriptor.
-}
treePrintDotGraph :: Tree -> Handle -> IO ()
treePrintDotGraph tree handle = do
  fileDescriptor <- fdFD <$> handleToFd handle
  withTreeAsTSTreePtr tree $ \treePtr ->
    C.ts_tree_print_dot_graph treePtr (coerce fileDescriptor)

-- * Node

-- | See @`C.ts_node_type`@.
nodeType :: Node -> IO ByteString
nodeType =
  -- The string returned by ts_node_type is null-terminated
  -- and lives in static and constant memory. Do not free.
  BSU.unsafePackCString <=< coerce C.ts_node_type
{-# INLINE nodeType #-}

-- | See @`C.ts_node_type`@.
nodeTypeAsString :: Node -> IO String
nodeTypeAsString = fmap BSC.unpack . nodeType
{-# INLINE nodeTypeAsString #-}

-- | See @`C.ts_node_symbol`@.
nodeSymbol :: Node -> IO Symbol
nodeSymbol = coerce C.ts_node_symbol
{-# INLINE nodeSymbol #-}

-- | See @`C.ts_node_language`@.
nodeLanguage :: Node -> IO Language
nodeLanguage = unsafeToLanguage <=< coerce C.ts_node_language
{-# INLINE nodeLanguage #-}

-- | See @`C.ts_node_grammar_type`@.
nodeGrammarType :: Node -> IO GrammarType
nodeGrammarType =
  -- The string returned by ts_node_grammar_type is null-terminated
  -- and lives in static and constant memory. Do not free.
  coerce . BSU.unsafePackCString <=< coerce C.ts_node_grammar_type
{-# INLINE nodeGrammarType #-}

-- | See @`C.ts_node_grammar_type`@.
nodeGrammarTypeAsString :: Node -> IO String
nodeGrammarTypeAsString = fmap (BSC.unpack . coerce) . nodeGrammarType
{-# INLINE nodeGrammarTypeAsString #-}

-- | See @`C.ts_node_grammar_symbol`@.
nodeGrammarSymbol :: Node -> IO Symbol
nodeGrammarSymbol = coerce C.ts_node_grammar_symbol
{-# INLINE nodeGrammarSymbol #-}

-- | Get the `Range` for the `Node`.
nodeRange :: Node -> IO Range
nodeRange node = do
  rangeStartByte <- nodeStartByte node
  rangeEndByte <- nodeEndByte node
  rangeStartPoint <- nodeStartPoint node
  rangeEndPoint <- nodeEndPoint node
  pure Range{..}

-- | See @`C.ts_node_start_byte`@.
nodeStartByte :: Node -> IO Word32
nodeStartByte = coerce C.ts_node_start_byte
{-# INLINE nodeStartByte #-}

-- | See @`C.ts_node_start_point`@.
nodeStartPoint :: Node -> IO Point
nodeStartPoint = coerce C.ts_node_start_point
{-# INLINE nodeStartPoint #-}

-- | See @`C.ts_node_end_byte`@.
nodeEndByte :: Node -> IO Word32
nodeEndByte = coerce C.ts_node_end_byte
{-# INLINE nodeEndByte #-}

-- | See @`C.ts_node_end_point`@.
nodeEndPoint :: Node -> IO Point
nodeEndPoint = coerce C.ts_node_end_point
{-# INLINE nodeEndPoint #-}

-- | See @`C.ts_node_string`@.
showNode :: Node -> IO ByteString
showNode = BSU.unsafePackMallocCString <=< coerce C.ts_node_string
{-# INLINE showNode #-}

-- | See @`C.ts_node_string`@.
showNodeAsString :: Node -> IO String
showNodeAsString = fmap BSC.unpack . showNode
{-# INLINE showNodeAsString #-}

-- | See @`C.ts_node_is_null`@.
nodeIsNull :: Node -> IO Bool
nodeIsNull = fmap toBool . C.ts_node_is_null . coerce
{-# INLINE nodeIsNull #-}

-- | See @`C.ts_node_is_named`@.
nodeIsNamed :: Node -> IO Bool
nodeIsNamed = fmap toBool . C.ts_node_is_named . coerce
{-# INLINE nodeIsNamed #-}

-- | See @`C.ts_node_is_missing`@.
nodeIsMissing :: Node -> IO Bool
nodeIsMissing = fmap toBool . C.ts_node_is_missing . coerce
{-# INLINE nodeIsMissing #-}

-- | See @`C.ts_node_is_extra`@.
nodeIsExtra :: Node -> IO Bool
nodeIsExtra = fmap toBool . C.ts_node_is_extra . coerce
{-# INLINE nodeIsExtra #-}

-- | See @`C.ts_node_has_changes`@.
nodeHasChanges :: Node -> IO Bool
nodeHasChanges = fmap toBool . C.ts_node_has_changes . coerce
{-# INLINE nodeHasChanges #-}

-- | See @`C.ts_node_has_error`@.
nodeHasError :: Node -> IO Bool
nodeHasError = fmap toBool . C.ts_node_has_error . coerce
{-# INLINE nodeHasError #-}

-- | See @`C.ts_node_is_error`@.
nodeIsError :: Node -> IO Bool
nodeIsError = fmap toBool . C.ts_node_is_error . coerce
{-# INLINE nodeIsError #-}

-- | See @`C.ts_node_parse_state`@.
nodeParseState :: Node -> IO StateId
nodeParseState = coerce C.ts_node_parse_state
{-# INLINE nodeParseState #-}

-- | See @`C.ts_node_next_parse_state`@.
nodeNextParseState :: Node -> IO StateId
nodeNextParseState = coerce C.ts_node_next_parse_state
{-# INLINE nodeNextParseState #-}

-- | See @`C.ts_node_parent`@.
nodeParent :: Node -> IO Node
nodeParent = coerce C.ts_node_parent
{-# INLINE nodeParent #-}

-- | See @`C.ts_node_child_with_descendant`@.
nodeChildWithDescendant :: Node -> Node -> IO Node
nodeChildWithDescendant = coerce C.ts_node_child_with_descendant
{-# INLINE nodeChildWithDescendant #-}

-- | See @`C.ts_node_child`@.
nodeChild :: Node -> Word32 -> IO Node
nodeChild = coerce C.ts_node_child
{-# INLINE nodeChild #-}

-- | See @`C.ts_node_field_name_for_child`@.
nodeFieldNameForChild :: Node -> Word32 -> IO FieldName
nodeFieldNameForChild node =
  -- The string returned by ts_node_grammar_type is null-terminated
  -- and lives in static and constant memory. Do not free.
  coerce . BSU.unsafePackCString <=< coerce C.ts_node_field_name_for_child node
{-# INLINE nodeFieldNameForChild #-}

-- | See @`C.ts_node_field_name_for_child`@.
nodeFieldNameForChildAsString :: Node -> Word32 -> IO String
nodeFieldNameForChildAsString node childIndex =
  BSC.unpack . coerce <$> nodeFieldNameForChild node childIndex
{-# INLINE nodeFieldNameForChildAsString #-}

-- | See @`C.ts_node_field_name_for_named_child`@.
nodeFieldNameForNamedChild :: Node -> Word32 -> IO FieldName
nodeFieldNameForNamedChild node =
  -- The string returned by ts_node_grammar_type is null-terminated
  -- and lives in static and constant memory. Do not free.
  coerce . BSU.unsafePackCString <=< coerce C.ts_node_field_name_for_named_child node
{-# INLINE nodeFieldNameForNamedChild #-}

-- | See @`C.ts_node_field_name_for_named_child`@.
nodeFieldNameForNamedChildAsString :: Node -> Word32 -> IO String
nodeFieldNameForNamedChildAsString node childIndex =
  BSC.unpack . coerce <$> nodeFieldNameForNamedChild node childIndex
{-# INLINE nodeFieldNameForNamedChildAsString #-}

-- | See @`C.ts_node_child_count`@.
nodeChildCount :: Node -> IO Word32
nodeChildCount = coerce C.ts_node_child_count
{-# INLINE nodeChildCount #-}

-- | See @`C.ts_node_named_child`@.
nodeNamedChild :: Node -> Word32 -> IO Node
nodeNamedChild = coerce C.ts_node_named_child
{-# INLINE nodeNamedChild #-}

-- | See @`C.ts_node_named_child_count`@.
nodeNamedChildCount :: Node -> IO Word32
nodeNamedChildCount = coerce C.ts_node_named_child_count
{-# INLINE nodeNamedChildCount #-}

-- | See @`C.ts_node_child_by_field_name`@.
nodeChildByFieldName :: Node -> FieldName -> IO Node
nodeChildByFieldName node fieldName =
  BSU.unsafeUseAsCStringLen (coerce fieldName) $ \(stringPtr, stringLen) ->
    coerce C.ts_node_child_by_field_name node stringPtr $
      fromIntegral @_ @Word32 stringLen
{-# INLINE nodeChildByFieldName #-}

-- | See @`C.ts_node_child_by_field_id`@.
nodeChildByFieldId :: Node -> FieldId -> IO Node
nodeChildByFieldId = coerce C.ts_node_child_by_field_id
{-# INLINE nodeChildByFieldId #-}

-- | See @`C.ts_node_next_sibling`@.
nodeNextSibling :: Node -> IO Node
nodeNextSibling = coerce C.ts_node_next_sibling
{-# INLINE nodeNextSibling #-}

-- | See @`C.ts_node_prev_sibling`@.
nodePrevSibling :: Node -> IO Node
nodePrevSibling = coerce C.ts_node_prev_sibling
{-# INLINE nodePrevSibling #-}

-- | See @`C.ts_node_next_named_sibling`@.
nodeNextNamedSibling :: Node -> IO Node
nodeNextNamedSibling = coerce C.ts_node_next_named_sibling
{-# INLINE nodeNextNamedSibling #-}

-- | See @`C.ts_node_prev_named_sibling`@.
nodePrevNamedSibling :: Node -> IO Node
nodePrevNamedSibling = coerce C.ts_node_prev_named_sibling
{-# INLINE nodePrevNamedSibling #-}

-- | See @`C.ts_node_first_child_for_byte`@.
nodeFirstChildForByte :: Node -> Word32 -> IO Node
nodeFirstChildForByte = coerce C.ts_node_first_child_for_byte
{-# INLINE nodeFirstChildForByte #-}

-- | See @`C.ts_node_first_named_child_for_byte`@.
nodeFirstNamedChildForByte :: Node -> Word32 -> IO Node
nodeFirstNamedChildForByte = coerce C.ts_node_first_named_child_for_byte
{-# INLINE nodeFirstNamedChildForByte #-}

-- | See @`C.ts_node_descendant_count`@.
nodeDescendantCount :: Node -> IO Word32
nodeDescendantCount = coerce C.ts_node_descendant_count
{-# INLINE nodeDescendantCount #-}

-- | See @`C.ts_node_descendant_for_byte_range`@.
nodeDescendantForByteRange :: Node -> Word32 -> Word32 -> IO Node
nodeDescendantForByteRange = coerce C.ts_node_descendant_for_byte_range
{-# INLINE nodeDescendantForByteRange #-}

-- | See @`C.ts_node_descendant_for_point_range`@.
nodeDescendantForPointRange :: Node -> Point -> Point -> IO Node
nodeDescendantForPointRange = coerce C.ts_node_descendant_for_point_range
{-# INLINE nodeDescendantForPointRange #-}

-- | See @`C.ts_node_named_descendant_for_byte_range`@.
nodeNamedDescendantForByteRange :: Node -> Word32 -> Word32 -> IO Node
nodeNamedDescendantForByteRange = coerce C.ts_node_named_descendant_for_byte_range
{-# INLINE nodeNamedDescendantForByteRange #-}

-- | See @`C.ts_node_named_descendant_for_point_range`@.
nodeNamedDescendantForPointRange :: Node -> Point -> Point -> IO Node
nodeNamedDescendantForPointRange = coerce C.ts_node_named_descendant_for_point_range
{-# INLINE nodeNamedDescendantForPointRange #-}

-- | See @`C.ts_node_edit`@.
nodeEdit :: Node -> InputEdit -> IO ()
nodeEdit node inputEdit =
  with (unWrapTSNode node) $ \nodePtr ->
    with (unWrapTSInputEdit inputEdit) $ \inputEditPtr ->
      coerce C.ts_node_edit nodePtr inputEditPtr
{-# INLINE nodeEdit #-}

-- | See @`C.ts_node_eq`@.
nodeEq :: Node -> Node -> IO Bool
nodeEq node1 node2 =
  toBool <$> C.ts_node_eq (coerce node1) (coerce node2)
{-# INLINE nodeEq #-}

-- * TreeCursor

withTreeCursorAsTSTreeCursorPtr :: TreeCursor -> (Ptr C.TSTreeCursor -> IO a) -> IO a
withTreeCursorAsTSTreeCursorPtr = withForeignPtr . coerce

-- | See @`C.ts_tree_cursor_new`@.
treeCursorNew :: Node -> IO TreeCursor
treeCursorNew node = do
  treeCursorForeignPtr <- mallocForeignPtr
  withForeignPtr treeCursorForeignPtr $ \treeCursorPtr ->
    C.ts_tree_cursor_new_p (coerce node) treeCursorPtr
  addForeignPtrFinalizer C.p_ts_tree_cursor_delete treeCursorForeignPtr
  pure $ WrapTSTreeCursor treeCursorForeignPtr

{-| Delete the 'TreeCursor'.

| See @`C.ts_tree_cursor_delete`@.

__Warning__: Using the 'Tree' after calling 'unsafeTreeCursorDelete' leads to undefined behaviour.
-}
unsafeTreeCursorDelete :: TreeCursor -> IO ()
unsafeTreeCursorDelete = coerce finalizeForeignPtr

-- | See @`C.ts_tree_cursor_reset`@.
treeCursorReset :: TreeCursor -> Node -> IO ()
treeCursorReset treeCursor node =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr ->
    C.ts_tree_cursor_reset treeCursorPtr (coerce node)

-- | See @`C.ts_tree_cursor_reset_to`@.
treeCursorResetTo :: TreeCursor -> TreeCursor -> IO ()
treeCursorResetTo self other =
  withTreeCursorAsTSTreeCursorPtr self $ \selfPtr ->
    withTreeCursorAsTSTreeCursorPtr other $ \otherPtr ->
      coerce C.ts_tree_cursor_reset_to selfPtr otherPtr

-- | See @`C.ts_tree_cursor_current_node`@.
treeCursorCurrentNode :: TreeCursor -> IO Node
treeCursorCurrentNode treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr ->
    coerce C.ts_tree_cursor_current_node treeCursorPtr

-- | See @`C.ts_tree_cursor_current_field_name`@.
treeCursorCurrentFieldName :: TreeCursor -> IO (Maybe ByteString)
treeCursorCurrentFieldName treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr -> do
    fieldNamePtr <- coerce C.ts_tree_cursor_current_field_name treeCursorPtr
    if fieldNamePtr == nullPtr
      then pure Nothing
      else Just <$> BSU.unsafePackCString fieldNamePtr

-- | See @`C.ts_tree_cursor_current_field_id`@.
treeCursorCurrentFieldId :: TreeCursor -> IO (Maybe FieldId)
treeCursorCurrentFieldId treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr -> do
    fieldId <- coerce C.ts_tree_cursor_current_field_id treeCursorPtr
    pure $ if fieldId == 0 then Nothing else Just fieldId

-- | See @`C.ts_tree_cursor_goto_parent`@.
treeCursorGotoParent :: TreeCursor -> IO Bool
treeCursorGotoParent treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    fmap toBool . C.ts_tree_cursor_goto_parent

-- | See @`C.ts_tree_cursor_goto_next_sibling`@.
treeCursorGotoNextSibling :: TreeCursor -> IO Bool
treeCursorGotoNextSibling treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    fmap toBool . C.ts_tree_cursor_goto_next_sibling

-- | See @`C.ts_tree_cursor_goto_previous_sibling`@.
treeCursorGotoPreviousSibling :: TreeCursor -> IO Bool
treeCursorGotoPreviousSibling treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    fmap toBool . C.ts_tree_cursor_goto_previous_sibling

-- | See @`C.ts_tree_cursor_goto_first_child`@.
treeCursorGotoFirstChild :: TreeCursor -> IO Bool
treeCursorGotoFirstChild treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    fmap toBool . C.ts_tree_cursor_goto_first_child

-- | See @`C.ts_tree_cursor_goto_last_child`@.
treeCursorGotoLastChild :: TreeCursor -> IO Bool
treeCursorGotoLastChild treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    fmap toBool . C.ts_tree_cursor_goto_last_child

-- | See @`C.ts_tree_cursor_goto_descendant`@.
treeCursorGotoDescendant :: TreeCursor -> Word32 -> IO ()
treeCursorGotoDescendant treeCursor descsendantIndex =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr ->
    C.ts_tree_cursor_goto_descendant treeCursorPtr descsendantIndex

-- | See @`C.ts_tree_cursor_current_descendant_index`@.
treeCursorCurrentDescendantIndex :: TreeCursor -> IO Word32
treeCursorCurrentDescendantIndex treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    C.ts_tree_cursor_current_descendant_index . coerce

-- | See @`C.ts_tree_cursor_current_depth`@.
treeCursorCurrentDepth :: TreeCursor -> IO Word32
treeCursorCurrentDepth treeCursor =
  withTreeCursorAsTSTreeCursorPtr treeCursor $
    C.ts_tree_cursor_current_depth . coerce

-- | See @`C.ts_tree_cursor_goto_first_child_for_byte`@.
treeCursorGotoFirstChildForByte :: TreeCursor -> Word32 -> IO Int64
treeCursorGotoFirstChildForByte treeCursor goalByte =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr ->
    C.ts_tree_cursor_goto_first_child_for_byte treeCursorPtr goalByte

-- | See @`C.ts_tree_cursor_goto_first_child_for_point`@.
treeCursorGotoFirstChildForPoint :: TreeCursor -> Point -> IO Int64
treeCursorGotoFirstChildForPoint treeCursor point =
  withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr ->
    C.ts_tree_cursor_goto_first_child_for_point treeCursorPtr (coerce point)

-- | See @`C.ts_tree_cursor_copy`@.
treeCursorCopy :: TreeCursor -> IO TreeCursor
treeCursorCopy treeCursor = do
  copyOfTreeCursorForeignPtr <- mallocForeignPtr
  withForeignPtr copyOfTreeCursorForeignPtr $ \copyOfTreeCursorPtr ->
    withTreeCursorAsTSTreeCursorPtr treeCursor $ \treeCursorPtr ->
      C.ts_tree_cursor_copy_p treeCursorPtr copyOfTreeCursorPtr
  addForeignPtrFinalizer C.p_ts_tree_cursor_delete copyOfTreeCursorForeignPtr
  pure $ WrapTSTreeCursor copyOfTreeCursorForeignPtr

-- * Query

withQueryAsTSQueryPtr :: Query -> (Ptr C.TSQuery -> IO a) -> IO a
withQueryAsTSQueryPtr = withForeignPtr . coerce

-- | See @`C.ts_query_new`@.
queryNew :: Language -> ByteString -> IO Query
queryNew language query =
  alloca $ \queryErrorOffsetPtr -> alloca $ \queryErrorTypePtr -> do
    queryPtr <-
      withLanguageAsTSLanguagePtr language $ \languagePtr ->
        BSU.unsafeUseAsCStringLen query $ \(queryPtr, queryLen) ->
          coerce C.ts_query_new languagePtr queryPtr (fromIntegral queryLen :: Word32) queryErrorOffsetPtr queryErrorTypePtr
    queryErrorType' <- peek queryErrorTypePtr
    if queryPtr /= nullPtr
      then assert (queryErrorType' == C.TSQueryErrorNone) $ do
        WrapTSQuery <$> newForeignPtr C.p_ts_query_delete queryPtr
      else assert (queryErrorType' /= C.TSQueryErrorNone) $ do
        queryErrorOffset' <- peek queryErrorOffsetPtr
        let !queryError = makeQueryError query queryErrorOffset' (WrapTSQueryErrorType queryErrorType')
        throwIO queryError

-- | Internal helper: Format a query error.
makeQueryError :: ByteString -> Word32 -> QueryErrorType -> QueryError
makeQueryError query queryErrorOffset queryErrorType = QueryError{..}
 where
  (queryErrorPoint, queryErrorLine) = offsetToPointAndLine query queryErrorOffset
  queryErrorMessage =
    BSC.unpack . BS.toStrict . BSB.toLazyByteString . mconcat $
      [ BSB.stringUtf8 $
          case queryErrorType of
            QueryErrorTypeSyntax -> "Invalid syntax"
            QueryErrorTypeNodeType -> "Invalid node type"
            QueryErrorTypeField -> "Invalid field name"
            QueryErrorTypeCapture -> "Invalid capture name"
            QueryErrorTypeStructure -> "Impossible pattern"
            QueryErrorTypeLanguage -> "Invalid language"
      , BSB.stringUtf8 " at row "
      , BSB.word32Dec $ pointRow queryErrorPoint
      , BSB.stringUtf8 ", column "
      , BSB.word32Dec $ pointColumn queryErrorPoint
      , BSB.stringUtf8 ":\n\n> "
      , BSB.stringUtf8 queryErrorLine
      , BSB.stringUtf8 "\n> "
      , BSB.stringUtf8 (replicate (fromIntegral $ pointColumn queryErrorPoint - 1) ' ')
      , BSB.stringUtf8 "^\n\n"
      ]

-- | Internal helper: Given a byte offset, find the row, column, and line pointed to.
offsetToPointAndLine :: ByteString -> Word32 -> (Point, String)
offsetToPointAndLine str offset = (point, line)
 where
  (prefix, suffix) = BS.splitAt (fromIntegral offset) str
  prefixLine = BSC.unpack $ BSC.takeWhileEnd (/= '\n') prefix
  suffixLine = BSC.unpack $ BSC.takeWhile (/= '\n') suffix
  row = 1 + BSC.count '\n' prefix
  column = 1 + length prefixLine
  point = Point{pointRow = fromIntegral row, pointColumn = fromIntegral column}
  line = prefixLine <> suffixLine

{-| Delete the 'Query' and free the associated memory.

See @`C.ts_query_delete`@.

__Warning__: Using the 'Query' after calling 'unsafeQueryDelete' leads to undefined behaviour.
-}
unsafeQueryDelete :: Query -> IO ()
unsafeQueryDelete = coerce finalizeForeignPtr

-- | See @`C.ts_query_pattern_count`@.
queryPatternCount :: Query -> IO Word32
queryPatternCount query =
  withQueryAsTSQueryPtr query $
    coerce C.ts_query_pattern_count

-- | See @`C.ts_query_capture_count`@.
queryCaptureCount :: Query -> IO Word32
queryCaptureCount query =
  withQueryAsTSQueryPtr query $
    coerce C.ts_query_capture_count

-- | See @`C.ts_query_string_count`@.
queryStringCount :: Query -> IO Word32
queryStringCount query =
  withQueryAsTSQueryPtr query $
    coerce C.ts_query_string_count

-- | See @`C.ts_query_start_byte_for_pattern`@.
queryStartByteForPattern :: Query -> PatternIndex -> IO Word32
queryStartByteForPattern query patternIndex =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    coerce C.ts_query_start_byte_for_pattern queryPtr patternIndex

-- | See @`C.ts_query_end_byte_for_pattern`@.
queryEndByteForPattern :: Query -> PatternIndex -> IO Word32
queryEndByteForPattern query patternIndex =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    coerce C.ts_query_end_byte_for_pattern queryPtr patternIndex

-- -- | See @`ts_query_predicates_for_pattern`@.
queryPredicatesForPattern :: Query -> Word32 -> IO [QueryPredicateStep]
queryPredicatesForPattern query patternIndex = do
  (stepsPtr, stepsLen) <-
    alloca $ \stepCountPtr -> do
      stepsPtr <-
        withQueryAsTSQueryPtr query $ \queryPtr ->
          C.ts_query_predicates_for_pattern (ConstPtr queryPtr) patternIndex stepCountPtr
      stepsLen <- fromIntegral <$> peek stepCountPtr
      pure (unConstPtr stepsPtr, stepsLen)
  coerce $ peekArray @C.TSQueryPredicateStep stepsLen stepsPtr

-- | See @`C.ts_query_is_pattern_rooted`@.
queryIsPatternRooted :: Query -> PatternIndex -> IO Bool
queryIsPatternRooted query patternIndex =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    toBool @CBool <$> coerce C.ts_query_is_pattern_rooted queryPtr patternIndex

-- | See @`C.ts_query_is_pattern_non_local`@.
queryIsPatternNonLocal :: Query -> PatternIndex -> IO Bool
queryIsPatternNonLocal query patternIndex =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    toBool @CBool <$> coerce C.ts_query_is_pattern_non_local queryPtr patternIndex

-- | See @`C.ts_query_is_pattern_guaranteed_at_step`@.
queryIsPatternGuaranteedAtStep :: Query -> Word32 -> IO Bool
queryIsPatternGuaranteedAtStep query byteOffset =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    toBool <$> C.ts_query_is_pattern_guaranteed_at_step (ConstPtr queryPtr) byteOffset

-- | See @`C.ts_query_capture_name_for_id`@.
queryCaptureNameForIndex :: Query -> CaptureIndex -> IO CaptureName
queryCaptureNameForIndex query captureIndex = do
  (namePtr, nameLen) <-
    alloca @Word32 $ \nameLenPtr ->
      (,)
        <$> withQueryAsTSQueryPtr
          query
          ( \queryPtr ->
              coerce C.ts_query_capture_name_for_id queryPtr captureIndex nameLenPtr
          )
        <*> (fromIntegral <$> peek nameLenPtr)
  coerce $ BSU.unsafePackCStringLen (namePtr, nameLen)

-- | See @`C.ts_query_capture_quantifier_for_id`@.
queryCaptureQuantifierForIndex :: Query -> PatternIndex -> CaptureIndex -> IO Quantifier
queryCaptureQuantifierForIndex query patternIndex captureIndex =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    coerce C.ts_query_capture_quantifier_for_id queryPtr patternIndex captureIndex

-- -- | See @`ts_query_string_value_for_id`@.
queryStringValueForIndex :: Query -> CaptureIndex -> IO ByteString
queryStringValueForIndex query captureIndex = do
  (strPtr, strLen) <-
    alloca @Word32 $ \strLenPtr ->
      (,)
        <$> withQueryAsTSQueryPtr
          query
          ( \queryPtr ->
              coerce C.ts_query_string_value_for_id queryPtr captureIndex strLenPtr
          )
        <*> (fromIntegral <$> peek strLenPtr)
  BS.packCStringLen (strPtr, strLen)

-- | See @`C.ts_query_disable_capture`@.
queryDisableCapture :: Query -> CaptureName -> IO ()
queryDisableCapture query captureName =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    BSU.unsafeUseAsCStringLen (coerce captureName) $ \(namePtr, nameLen) ->
      coerce C.ts_query_disable_capture queryPtr namePtr (fromIntegral nameLen :: Word32)

-- | See @`C.ts_query_disable_pattern`@.
queryDisablePattern :: Query -> PatternIndex -> IO ()
queryDisablePattern query patternIndex =
  withQueryAsTSQueryPtr query $ \queryPtr ->
    coerce C.ts_query_disable_pattern queryPtr patternIndex

withQueryCursorAsTSQueryCursorPtr :: QueryCursor -> (Ptr C.TSQueryCursor -> IO a) -> IO a
withQueryCursorAsTSQueryCursorPtr = withForeignPtr . coerce

-- | See @`C.ts_query_cursor_new`@.
queryCursorNew :: IO QueryCursor
queryCursorNew =
  coerce newForeignPtr C.p_ts_query_cursor_delete =<< C.ts_query_cursor_new

-- | See @`C.ts_query_cursor_delete`@.
unsafeQueryCursorDelete :: QueryCursor -> IO ()
unsafeQueryCursorDelete = coerce finalizeForeignPtr

-- | See @`C.ts_query_cursor_exec`@.
queryCursorExec :: QueryCursor -> Query -> Node -> IO ()
queryCursorExec queryCursor query node =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    withQueryAsTSQueryPtr query $ \queryPtr ->
      coerce C.ts_query_cursor_exec queryCursorPtr queryPtr node

-- | See @`C.ts_query_cursor_did_exceed_match_limit`@.
queryCursorDidExceedMatchLimit :: QueryCursor -> IO Bool
queryCursorDidExceedMatchLimit queryCursor =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    toBool @CBool <$> coerce C.ts_query_cursor_did_exceed_match_limit queryCursorPtr

-- | See @`C.ts_query_cursor_match_limit`@.
queryCursorMatchLimit :: QueryCursor -> IO Word32
queryCursorMatchLimit queryCursor =
  withQueryCursorAsTSQueryCursorPtr queryCursor $
    coerce C.ts_query_cursor_match_limit

-- | See @`C.ts_query_cursor_set_match_limit`@.
queryCursorSetMatchLimit :: QueryCursor -> Word32 -> IO ()
queryCursorSetMatchLimit queryCursor matchLimit =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    C.ts_query_cursor_set_match_limit queryCursorPtr matchLimit

-- | See @`C.ts_query_cursor_set_timeout_micros`@.
queryCursorSetTimeoutMicros :: QueryCursor -> Microsecond -> IO ()
queryCursorSetTimeoutMicros queryCursor micros =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    coerce C.ts_query_cursor_set_timeout_micros queryCursorPtr micros

-- | See @`C.ts_query_cursor_timeout_micros`@.
queryCursorTimeoutMicros :: QueryCursor -> IO Microsecond
queryCursorTimeoutMicros queryCursor =
  withQueryCursorAsTSQueryCursorPtr queryCursor $
    coerce C.ts_query_cursor_timeout_micros

-- | See @`C.ts_query_cursor_set_byte_range`@.
queryCursorSetByteRange :: QueryCursor -> Word32 -> Word32 -> IO ()
queryCursorSetByteRange queryCursor startByte endByte =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    C.ts_query_cursor_set_byte_range queryCursorPtr startByte endByte

-- | See @`C.ts_query_cursor_set_point_range`@.
queryCursorSetPointRange :: QueryCursor -> Point -> Point -> IO ()
queryCursorSetPointRange queryCursor startPoint endPoint =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    coerce C.ts_query_cursor_set_point_range queryCursorPtr startPoint endPoint

-- | See @`C.ts_query_cursor_next_match`@.
queryCursorNextMatch :: QueryCursor -> IO (Maybe QueryMatch)
queryCursorNextMatch queryCursor =
  alloca $ \queryMatchPtr -> do
    success <-
      withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
        C.ts_query_cursor_next_match queryCursorPtr queryMatchPtr
    if toBool success
      then do
        queryMatch <- peek queryMatchPtr
        pure . Just . coerce $ queryMatch
      else pure Nothing

-- | See @`C.ts_query_cursor_remove_match`@.
queryCursorRemoveMatch :: QueryCursor -> CaptureIndex -> IO ()
queryCursorRemoveMatch queryCursor captureIndex =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    coerce C.ts_query_cursor_remove_match queryCursorPtr captureIndex

-- | See @`C.ts_query_cursor_next_capture`@.
queryCursorNextCapture :: QueryCursor -> IO (Maybe (CaptureIndex, QueryMatch))
queryCursorNextCapture queryCursor =
  alloca $ \captureIndexPtr -> alloca $ \queryMatchPtr -> do
    success <-
      withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
        C.ts_query_cursor_next_capture queryCursorPtr queryMatchPtr captureIndexPtr
    if toBool success
      then do
        captureIndex <- peek captureIndexPtr
        queryMatch <- peek queryMatchPtr
        pure . Just . coerce $ (captureIndex, queryMatch)
      else pure Nothing

-- | See @`C.ts_query_cursor_set_max_start_depth`@.
queryCursorSetMaxStartDepth :: QueryCursor -> Word32 -> IO ()
queryCursorSetMaxStartDepth queryCursor maxStartDepth =
  withQueryCursorAsTSQueryCursorPtr queryCursor $ \queryCursorPtr ->
    C.ts_query_cursor_set_max_start_depth queryCursorPtr maxStartDepth

-- * Language

unsafeToLanguage :: ConstPtr tsLanguage -> IO Language
unsafeToLanguage languageConstPtr = do
  let languagePtr = castPtr . unConstPtr $ languageConstPtr
  let languageFinalizer = castFunPtr C.p_ts_language_delete
  languageForeignPtr <- newForeignPtr languageFinalizer languagePtr
  pure $ coerce languageForeignPtr

unsafeLanguageDelete :: Language -> IO ()
unsafeLanguageDelete =
  finalizeForeignPtr . coerce

withLanguageAsTSLanguagePtr :: Language -> (ConstPtr C.TSLanguage -> IO a) -> IO a
withLanguageAsTSLanguagePtr language action =
  withForeignPtr (coerce language) (action . ConstPtr)

-- | See @`C.ts_language_copy`@.
languageCopy :: Language -> IO Language
languageCopy language =
  withLanguageAsTSLanguagePtr language $
    unsafeToLanguage <=< C.ts_language_copy . coerce

-- | See @`C.ts_language_symbol_count`@.
languageSymbolCount :: Language -> IO Word32
languageSymbolCount language =
  withLanguageAsTSLanguagePtr
    language
    C.ts_language_symbol_count

-- | See @`C.ts_language_state_count`@.
languageStateCount :: Language -> IO Word32
languageStateCount language =
  withLanguageAsTSLanguagePtr
    language
    C.ts_language_state_count

-- | See @`C.ts_language_symbol_name`@.
languageSymbolName :: Language -> Symbol -> IO ByteString
languageSymbolName language symbol =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    BSU.unsafePackCString =<< coerce C.ts_language_symbol_name languagePtr symbol

-- | See @`C.ts_language_symbol_for_name`@.
languageSymbolForGrammarType :: Language -> ByteString -> Bool -> IO Symbol
languageSymbolForGrammarType language grammarType isNamed =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    BSU.unsafeUseAsCStringLen grammarType $ \(grammarTypeStr, grammarTypeLen) ->
      coerce
        C.ts_language_symbol_for_name
        languagePtr
        grammarTypeStr
        (fromIntegral grammarTypeLen :: Word32)
        (fromBool isNamed :: CBool)

-- | See @`C.ts_language_field_count`@.
languageFieldCount :: Language -> IO Word32
languageFieldCount language =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    C.ts_language_field_count languagePtr

-- | See @`C.ts_language_field_name_for_id`@.
languageFieldNameForId :: Language -> FieldId -> IO ByteString
languageFieldNameForId language fieldId =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    BSU.unsafePackCString =<< coerce C.ts_language_field_name_for_id languagePtr fieldId

-- | See @`C.ts_language_field_id_for_name`@.
languageFieldIdForName :: Language -> ByteString -> IO FieldId
languageFieldIdForName language fieldName =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    BSU.unsafeUseAsCStringLen fieldName $ \(fieldNameStr, fieldNameLen) ->
      coerce
        C.ts_language_field_id_for_name
        languagePtr
        fieldNameStr
        (fromIntegral fieldNameLen :: Word32)

-- | See @`C.ts_language_symbol_type`@.
languageSymbolType :: Language -> Symbol -> IO SymbolType
languageSymbolType language symbol =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    coerce C.ts_language_symbol_type languagePtr symbol

-- | See @`C.ts_language_version`@.
languageVersion :: Language -> IO Word32
languageVersion language =
  withLanguageAsTSLanguagePtr language C.ts_language_version

-- | See @`C.ts_language_next_state`@.
languageNextState :: Language -> StateId -> Symbol -> IO StateId
languageNextState language stateId symbol =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    coerce C.ts_language_next_state languagePtr stateId symbol

-- * Lookahead Iterator

withLookaheadIteratorAsTSLookaheadIteratorPtr :: LookaheadIterator -> (ConstPtr C.TSLookaheadIterator -> IO a) -> IO a
withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator action =
  withForeignPtr (coerce lookaheadIterator) (action . ConstPtr)

-- | See @`C.ts_lookahead_iterator_new`@.
lookaheadIteratorNew :: Language -> StateId -> IO LookaheadIterator
lookaheadIteratorNew language stateId =
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    coerce newForeignPtr C.p_ts_lookahead_iterator_delete
      =<< C.ts_lookahead_iterator_new languagePtr (coerce stateId)

{-| See @`C.ts_lookahead_iterator_delete`@.

    __Warning__: Using the `LookaheadIterator` after calling `unsafeLookaheadIteratorDelete` leads to undefined behaviour.
-}
unsafeLookaheadIteratorDelete :: LookaheadIterator -> IO ()
unsafeLookaheadIteratorDelete = coerce finalizeForeignPtr

-- | See @`C.ts_lookahead_iterator_reset_state`@.
lookaheadIteratorResetState :: LookaheadIterator -> StateId -> IO Bool
lookaheadIteratorResetState lookaheadIterator stateId =
  withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator $ \lookaheadIteratorPtr -> do
    toBool @CBool <$> coerce C.ts_lookahead_iterator_reset_state lookaheadIteratorPtr stateId

-- | See @`C.ts_lookahead_iterator_reset`@.
lookaheadIteratorReset :: LookaheadIterator -> Language -> StateId -> IO Bool
lookaheadIteratorReset lookaheadIterator language stateId = do
  withLanguageAsTSLanguagePtr language $ \languagePtr ->
    withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator $ \lookaheadIteratorPtr ->
      toBool @CBool <$> coerce C.ts_lookahead_iterator_reset lookaheadIteratorPtr languagePtr stateId

-- | See @`C.ts_lookahead_iterator_language`@.
lookaheadIteratorLanguage :: LookaheadIterator -> IO Language
lookaheadIteratorLanguage lookaheadIterator =
  withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator $
    unsafeToLanguage <=< coerce C.ts_lookahead_iterator_language

-- | See @`C.ts_lookahead_iterator_next`@.
lookaheadIteratorNext :: LookaheadIterator -> IO Bool
lookaheadIteratorNext lookaheadIterator =
  withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator $ \lookaheadIteratorPtr ->
    toBool @CBool <$> coerce C.ts_lookahead_iterator_next lookaheadIteratorPtr

-- | See @`C.ts_lookahead_iterator_current_symbol`@.
lookaheadIteratorCurrentSymbol :: LookaheadIterator -> IO Symbol
lookaheadIteratorCurrentSymbol lookaheadIterator =
  withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator $ \lookaheadIteratorPtr ->
    coerce C.ts_lookahead_iterator_current_symbol lookaheadIteratorPtr

-- | See @`C.ts_lookahead_iterator_current_symbol_name`@.
lookaheadIteratorCurrentSymbolName :: LookaheadIterator -> IO ByteString
lookaheadIteratorCurrentSymbolName lookaheadIterator =
  withLookaheadIteratorAsTSLookaheadIteratorPtr lookaheadIterator $
    BSU.unsafePackCString <=< coerce C.ts_lookahead_iterator_current_symbol_name

-- * Global Configuration

-- -- | See @`C.ts_set_allocator`@.
-- setAllocator :: FunPtr (CSize -> IO ()) -> FunPtr (CSize -> CSize -> IO ()) -> FunPtr (Ptr a -> CSize -> IO ()) -> FunPtr (Ptr a -> IO ()) -> IO ()
-- setAllocator = undefined -- C.ts_set_allocator
