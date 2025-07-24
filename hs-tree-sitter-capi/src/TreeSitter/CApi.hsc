{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module TreeSitter.CApi
  ( -- * ABI Versioning
    TREE_SITTER_LANGUAGE_VERSION
  , TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION

    -- * Types
  , TSStateId (..)
  , TSSymbol (..)
  , TSFieldId (..)
  , TSLanguage
  , TSParser
  , TSTree
  , TSQuery
  , TSQueryCursor
  , TSLookaheadIterator
  , TSInputEncoding (TSInputEncodingUTF8, TSInputEncodingUTF16, ..)
  , TSSymbolType (TSSymbolTypeRegular, TSSymbolTypeAnonymous, TSSymbolTypeSupertype, TSSymbolTypeAuxiliary, ..)
  , TSPoint (..)
  , TSRange (..)
  , TSInput
  , TSRead
  , TSLogType (TSLogTypeParse, TSLogTypeLex, ..)
  , TSLogger
  , TSLog
  , TSInputEdit (..)
  , TSNode (..)
  , TSNodeContext (..)
  , TSTreeCursor (..)
  , TSTreeCursorContext (..)
  , TSQueryCapture (..)
  , TSQuantifier (TSQuantifierZero, TSQuantifierZeroOrOne, TSQuantifierZeroOrMore, TSQuantifierOne, TSQuantifierOneOrMore, ..)
  , TSQueryMatch (..)
  , TSQueryPredicateStepType (TSQueryPredicateStepTypeDone, TSQueryPredicateStepTypeCapture, TSQueryPredicateStepTypeString, ..)
  , TSQueryPredicateStep (..)
  , TSQueryError (TSQueryErrorNone, TSQueryErrorSyntax, TSQueryErrorNodeType, TSQueryErrorField, TSQueryErrorCapture, TSQueryErrorStructure, TSQueryErrorLanguage, ..)

    -- * Parser
  , ts_parser_new
  , ts_parser_delete
  , p_ts_parser_delete
  , ts_parser_language
  , ts_parser_set_language
  , ts_parser_set_included_ranges
  , ts_parser_included_ranges
  , ts_parser_set_logger
  , ts_parser_logger
  , ts_parser_remove_logger
  , ts_parser_parse
  , ts_parser_parse_string
  , ts_parser_parse_string_encoding
  , ts_parser_reset
  , ts_parser_set_timeout_micros
  , ts_parser_timeout_micros
  , ts_parser_set_cancellation_flag
  , ts_parser_cancellation_flag
  , ts_parser_print_dot_graphs

    -- * Tree
  , ts_tree_copy
  , ts_tree_delete
  , p_ts_tree_delete
  , ts_tree_language
  , ts_tree_included_ranges
  , ts_tree_edit
  , ts_tree_get_changed_ranges
  , ts_tree_print_dot_graph
  , ts_tree_root_node
  , ts_tree_root_node_with_offset

    -- * Node
  , ts_node_type
  , ts_node_symbol
  , ts_node_language
  , ts_node_grammar_type
  , ts_node_grammar_symbol
  , ts_node_start_byte
  , ts_node_start_point
  , ts_node_end_byte
  , ts_node_end_point
  , ts_node_string
  , ts_node_is_null
  , ts_node_is_named
  , ts_node_is_missing
  , ts_node_is_extra
  , ts_node_has_changes
  , ts_node_has_error
  , ts_node_is_error
  , ts_node_parse_state
  , ts_node_next_parse_state
  , ts_node_parent
  , ts_node_child_with_descendant
  , ts_node_child
  , ts_node_field_name_for_child
  , ts_node_field_name_for_named_child
  , ts_node_child_count
  , ts_node_named_child
  , ts_node_named_child_count
  , ts_node_child_by_field_name
  , ts_node_child_by_field_id
  , ts_node_next_sibling
  , ts_node_prev_sibling
  , ts_node_next_named_sibling
  , ts_node_prev_named_sibling
  , ts_node_first_child_for_byte
  , ts_node_first_named_child_for_byte
  , ts_node_descendant_count
  , ts_node_descendant_for_byte_range
  , ts_node_descendant_for_point_range
  , ts_node_named_descendant_for_byte_range
  , ts_node_named_descendant_for_point_range
  , ts_node_edit
  , ts_node_eq

    -- * TreeCursor
  , ts_tree_cursor_new
  , ts_tree_cursor_new_p
  , ts_tree_cursor_delete
  , p_ts_tree_cursor_delete
  , ts_tree_cursor_reset
  , ts_tree_cursor_reset_to
  , ts_tree_cursor_current_node
  , ts_tree_cursor_current_field_name
  , ts_tree_cursor_current_field_id
  , ts_tree_cursor_goto_parent
  , ts_tree_cursor_goto_next_sibling
  , ts_tree_cursor_goto_previous_sibling
  , ts_tree_cursor_goto_first_child
  , ts_tree_cursor_goto_last_child
  , ts_tree_cursor_goto_descendant
  , ts_tree_cursor_current_descendant_index
  , ts_tree_cursor_current_depth
  , ts_tree_cursor_goto_first_child_for_byte
  , ts_tree_cursor_goto_first_child_for_point
  , ts_tree_cursor_copy
  , ts_tree_cursor_copy_p

    -- * Query
  , ts_query_new
  , ts_query_delete
  , p_ts_query_delete
  , ts_query_pattern_count
  , ts_query_capture_count
  , ts_query_string_count
  , ts_query_start_byte_for_pattern
  , ts_query_end_byte_for_pattern
  , ts_query_predicates_for_pattern
  , ts_query_is_pattern_rooted
  , ts_query_is_pattern_non_local
  , ts_query_is_pattern_guaranteed_at_step
  , ts_query_capture_name_for_id
  , ts_query_capture_quantifier_for_id
  , ts_query_string_value_for_id
  , ts_query_disable_capture
  , ts_query_disable_pattern
  , ts_query_cursor_new
  , ts_query_cursor_delete
  , p_ts_query_cursor_delete
  , ts_query_cursor_exec
  , ts_query_cursor_did_exceed_match_limit
  , ts_query_cursor_match_limit
  , ts_query_cursor_set_match_limit
  , ts_query_cursor_set_timeout_micros
  , ts_query_cursor_timeout_micros
  , ts_query_cursor_set_byte_range
  , ts_query_cursor_set_point_range
  , ts_query_cursor_next_match
  , ts_query_cursor_remove_match
  , ts_query_cursor_next_capture
  , ts_query_cursor_set_max_start_depth

    -- * Language
  , ts_language_copy
  , ts_language_delete
  , p_ts_language_delete
  , ts_language_symbol_count
  , ts_language_state_count
  , ts_language_symbol_name
  , ts_language_symbol_for_name
  , ts_language_field_count
  , ts_language_field_name_for_id
  , ts_language_field_id_for_name
  , ts_language_symbol_type
  , ts_language_version
  , ts_language_next_state

    -- * Lookahead Iterator
  , ts_lookahead_iterator_new
  , ts_lookahead_iterator_delete
  , p_ts_lookahead_iterator_delete
  , ts_lookahead_iterator_reset_state
  , ts_lookahead_iterator_reset
  , ts_lookahead_iterator_language
  , ts_lookahead_iterator_next
  , ts_lookahead_iterator_current_symbol
  , ts_lookahead_iterator_current_symbol_name

#ifdef TREE_SITTER_FEATURE_WASM
    -- * WebAssembly Integration
  , TSWasmEngine
  , TSWasmStore
  , TSWasmErrorKind (TSWasmErrorKindNone, TSWasmErrorKindParse, TSWasmErrorKindCompile, TSWasmErrorKindInstantiate, TSWasmErrorKindAllocate)
  , TSWasmError
  , peekTSWasmError
  , withTSWasmError
  , ts_wasm_store_new
  , ts_wasm_store_delete
  , p_ts_wasm_store_delete
  , ts_wasm_store_load_language
  , ts_wasm_store_language_count
  , ts_language_is_wasm
  , ts_parser_set_wasm_store
  , ts_parser_take_wasm_store
#endif

    -- * Global Configuration
  , ts_set_allocator
  ) where

import Control.Exception (bracket, mask_)
#ifdef TREE_SITTER_FEATURE_WASM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (packCString, useAsCString)
#endif
import Data.Void (Void)
import Data.Word
import Foreign
import Foreign.C
import Foreign.C.ConstPtr.Compat (ConstPtr(..))
import GHC.TypeLits (Nat)

#include <tree_sitter/api.h>
-- string.h: required for memcpy
#include <string.h>

{----------------------------}
{- Section - ABI Versioning -}
{----------------------------}

{-|
  The latest ABI version that is supported by the current version of the
  library. When Languages are generated by the Tree-sitter CLI, they are
  assigned an ABI version number that corresponds to the current CLI version.
  The Tree-sitter library is generally backwards-compatible with languages
  generated using older CLI versions, but is not forwards-compatible.

  > #define TREE_SITTER_LANGUAGE_VERSION 14
-}
type TREE_SITTER_LANGUAGE_VERSION :: Nat
type TREE_SITTER_LANGUAGE_VERSION = #const TREE_SITTER_LANGUAGE_VERSION

{-|
  The earliest ABI version that is supported by the current version of the
  library.

  > #define TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION 13
-}
type TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION :: Nat
type TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION = #const TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION

{-------------------}
{- Section - Types -}
{-------------------}

{-|
  > typedef uint16_t TSStateId;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSStateId" #-}
  TSStateId = TSStateId #{type TSStateId}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

{-|
  > typedef uint16_t TSSymbol;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSSymbol" #-}
  TSSymbol = TSSymbol #{type TSSymbol}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

{-|
  > typedef uint16_t TSFieldId;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSFieldId" #-}
  TSFieldId = TSFieldId #{type TSFieldId}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Real, Integral, Enum)

{-|
  > typedef struct TSLanguage TSLanguage;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSLanguage" #-}
  TSLanguage

{-|
  > typedef struct TSParser TSParser;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSParser" #-}
  TSParser

{-|
  > typedef struct TSTree TSTree;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSTree" #-}
  TSTree

{-|
  > typedef struct TSQuery TSQuery;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSQuery" #-}
  TSQuery

{-|
  > typedef struct TSQueryCursor TSQueryCursor;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSQueryCursor" #-}
  TSQueryCursor

{-|
  > typedef struct TSLookaheadIterator TSLookaheadIterator;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSLookaheadIterator" #-}
  TSLookaheadIterator

{-|
  > typedef enum TSInputEncoding {
  >   TSInputEncodingUTF8,
  >   TSInputEncodingUTF16,
  > } TSInputEncoding;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSInputEncoding" #-}
  TSInputEncoding = TSInputEncoding
    { unTSInputEncoding :: #{type TSInputEncoding}
    }
    deriving (Eq, Show)
    deriving newtype (Storable)

pattern TSInputEncodingUTF8 :: TSInputEncoding
pattern TSInputEncodingUTF8 = TSInputEncoding ( #{const TSInputEncodingUTF8} )

pattern TSInputEncodingUTF16 :: TSInputEncoding
pattern TSInputEncodingUTF16 = TSInputEncoding ( #{const TSInputEncodingUTF16} )

{-# COMPLETE TSInputEncodingUTF8, TSInputEncodingUTF16 #-}

{-|
  > typedef enum TSSymbolType {
  >   TSSymbolTypeRegular,
  >   TSSymbolTypeAnonymous,
  >   TSSymbolTypeSupertype,
  >   TSSymbolTypeAuxiliary,
  > } TSSymbolType;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSSymbolType" #-}
  TSSymbolType = TSSymbolType
    { unTSSymbolType :: #{type TSSymbolType}
    }
    deriving (Eq, Show)

pattern TSSymbolTypeRegular :: TSSymbolType
pattern TSSymbolTypeRegular = TSSymbolType ( #{const TSSymbolTypeRegular} )

pattern TSSymbolTypeAnonymous :: TSSymbolType
pattern TSSymbolTypeAnonymous = TSSymbolType ( #{const TSSymbolTypeAnonymous} )

pattern TSSymbolTypeSupertype :: TSSymbolType
pattern TSSymbolTypeSupertype = TSSymbolType ( #{const TSSymbolTypeSupertype} )

pattern TSSymbolTypeAuxiliary :: TSSymbolType
pattern TSSymbolTypeAuxiliary = TSSymbolType ( #{const TSSymbolTypeAuxiliary} )

{-# COMPLETE TSSymbolTypeRegular, TSSymbolTypeAnonymous, TSSymbolTypeSupertype, TSSymbolTypeAuxiliary #-}

{-|
  > typedef struct TSPoint {
  >   uint32_t row;
  >   uint32_t column;
  > } TSPoint;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSPoint" #-}
  TSPoint = TSPoint
  { row    :: {-# UNPACK #-} !( #{type uint32_t} )
  , column :: {-# UNPACK #-} !( #{type uint32_t} )
  }
  deriving (Ord, Eq, Show)

instance Storable TSPoint where
  alignment _ = #{alignment TSPoint}
  sizeOf _ = #{size TSPoint}
  peek ptr = do
    row <- #{peek TSPoint, row} ptr
    column <- #{peek TSPoint, column} ptr
    return TSPoint{..}
  poke ptr TSPoint{..} = do
    #{poke TSPoint, row} ptr row
    #{poke TSPoint, column} ptr column

{-|
  > typedef struct TSRange {
  >   TSPoint start_point;
  >   TSPoint end_point;
  >   uint32_t start_byte;
  >   uint32_t end_byte;
  > } TSRange;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSRange" #-}
  TSRange = TSRange
  { start_point :: {-# UNPACK #-} !TSPoint
  , end_point   :: {-# UNPACK #-} !TSPoint
  , start_byte  :: {-# UNPACK #-} !( #{type uint32_t} )
  , end_byte    :: {-# UNPACK #-} !( #{type uint32_t} )
  }
  deriving (Eq, Show)

instance Storable TSRange where
  alignment _ = #{alignment TSRange}
  sizeOf _ = #{size TSRange}
  peek ptr = do
    start_point <- #{peek TSRange, start_point} ptr
    end_point <- #{peek TSRange, end_point} ptr
    start_byte <- #{peek TSRange, start_byte} ptr
    end_byte <- #{peek TSRange, end_byte} ptr
    return TSRange{..}
  poke ptr TSRange{..} = do
    #{poke TSRange, start_point} ptr start_point
    #{poke TSRange, end_point} ptr end_point
    #{poke TSRange, start_byte} ptr start_byte
    #{poke TSRange, end_byte} ptr end_byte

{-|
  > typedef struct TSInput {
  >   void *payload;
  >   const char *(*read)(
  >     void *payload,
  >     uint32_t byte_index,
  >     TSPoint position,
  >     uint32_t *bytes_read
  >   );
  >   TSInputEncoding encoding;
  > } TSInput;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSInput" #-}
  TSInput

{-| The type of the @`read`@ argument of the @`_wrap_ts_input_new`@ function.

  > typedef const char *(*TSRead)(
  >   uint32_t byte_index,
  >   TSPoint *position,
  >   uint32_t *bytes_read
  > );
  -}
type TSRead =
  ( #{type uint32_t} ) ->
  Ptr TSPoint ->
  Ptr ( #{type uint32_t} ) ->
  IO (ConstPtr CChar)

-- | Convert a Haskell 'TSRead' closure to a C 'TSRead' function pointer.
foreign import ccall "wrapper"
  mkTSReadFunPtr :: TSRead -> IO (FunPtr TSRead)

#{def
  typedef const char *(*TSRead)(
    uint32_t byte_index,
    TSPoint *position,
    uint32_t *bytes_read
  );
}

-- | Create a @`TSInput`@.
foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_input_new"
  _wrap_ts_input_new ::
    FunPtr TSRead ->
    TSInputEncoding ->
    IO (Ptr TSInput)

#{def
  const char *_wrap_ts_input_read(
    void *payload,
    uint32_t byte_index,
    TSPoint position,
    uint32_t *bytes_read
  ) {
    TSRead read;
    memcpy(&read, payload, sizeof read);
    TSPoint *position_p = &position;
    return read(byte_index, position_p, bytes_read);
  }
}

#{def
  TSInput *_wrap_ts_input_new(
    TSRead read,
    TSInputEncoding encoding
  ) {
    TSInput *input = malloc(sizeof *input);
    input->payload = malloc(sizeof read);
    memcpy(input->payload, &read, sizeof read);
    input->read = _wrap_ts_input_read;
    input->encoding = encoding;
    return input;
  }
}

-- | Delete a @`TSInput`@.
foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_input_delete"
  _wrap_ts_input_delete ::
    Ptr TSInput ->
    IO ()

#{def
  void _wrap_ts_input_delete(
    TSInput *input
  ) {
    free(input->payload);
    free(input);
  }
}

{-|
  > typedef enum TSLogType {
  >   TSLogTypeParse,
  >   TSLogTypeLex,
  > } TSLogType;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSLogType" #-}
  TSLogType = TSLogType
    { unTSLogType :: #{type TSLogType}
    }
    deriving (Eq, Show)

pattern TSLogTypeParse :: TSLogType
pattern TSLogTypeParse = TSLogType ( #{const TSLogTypeParse} )

pattern TSLogTypeLex :: TSLogType
pattern TSLogTypeLex = TSLogType ( #{const TSLogTypeLex} )

{-# COMPLETE TSLogTypeParse, TSLogTypeLex #-}

{-|
  > typedef struct TSLogger {
  >   void *payload;
  >   void (*log)(void *payload, TSLogType log_type, const char *buffer);
  > } TSLogger;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSLogger" #-}
  TSLogger

#{def
typedef void (*TSLog)(
  TSLogType log_type,
  const char *buffer
);
}

{-| The type of the @`log`@ argument of the @`_wrap_ts_logger_new`@ function.

  > void (*log)(TSLogType log_type, const char *buffer);
 -}
type TSLog =
  TSLogType ->
  ConstPtr CChar ->
  IO ()

{-|
 > TSLogger *_wrap_ts_logger_new(TSLog log);
 -}
foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_logger_new"
  _wrap_ts_logger_new ::
    FunPtr TSLog ->
    IO (Ptr TSLogger)

#{def
  void _wrap_ts_logger_log(
    void *payload,
    TSLogType log_type,
    const char *buffer
  ) {
    TSLog log;
    memcpy(&log, payload, sizeof log);
    log(log_type, buffer);
  }
}

#{def
  TSLogger *_wrap_ts_logger_new(TSLog log) {
    TSLogger *logger = malloc(sizeof *logger);
    logger->payload = malloc(sizeof log);
    memcpy(logger->payload, &log, sizeof log);
    logger->log = _wrap_ts_logger_log;
    return logger;
  }
}

{-| Allocate a C function pointer for a `TSLog` function.
 -}
foreign import ccall "wrapper"
  mkTSLogFunPtr :: TSLog -> IO (FunPtr TSLog)

{-| Convert a C function pointer for a `TSLog` function
    to the corresponding Haskell function.
 -}
foreign import ccall "dynamic"
  unTSLogFunPtr :: FunPtr TSLog -> TSLog

{-|
  > typedef struct TSInputEdit {
  >   uint32_t start_byte;
  >   uint32_t old_end_byte;
  >   uint32_t new_end_byte;
  >   TSPoint start_point;
  >   TSPoint old_end_point;
  >   TSPoint new_end_point;
  > } TSInputEdit;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSInputEdit" #-}
  TSInputEdit = TSInputEdit
  { start_byte    :: {-# UNPACK #-} !( #{type uint32_t} )
  , old_end_byte  :: {-# UNPACK #-} !( #{type uint32_t} )
  , new_end_byte  :: {-# UNPACK #-} !( #{type uint32_t} )
  , start_point   :: {-# UNPACK #-} !TSPoint
  , old_end_point :: {-# UNPACK #-} !TSPoint
  , new_end_point :: {-# UNPACK #-} !TSPoint
  }
  deriving (Eq, Show)

instance Storable TSInputEdit where
  alignment _ = #{alignment TSInputEdit}
  sizeOf _ = #{size TSInputEdit}
  peek ptr = do
    start_byte <- #{peek TSInputEdit, start_byte} ptr
    old_end_byte <- #{peek TSInputEdit, old_end_byte} ptr
    new_end_byte <- #{peek TSInputEdit, new_end_byte} ptr
    start_point <- #{peek TSInputEdit, start_point} ptr
    old_end_point <- #{peek TSInputEdit, old_end_point} ptr
    new_end_point <- #{peek TSInputEdit, new_end_point} ptr
    return TSInputEdit{..}
  poke ptr TSInputEdit{..} = do
    #{poke TSInputEdit, start_byte} ptr start_byte
    #{poke TSInputEdit, old_end_byte} ptr old_end_byte
    #{poke TSInputEdit, new_end_byte} ptr new_end_byte
    #{poke TSInputEdit, start_point} ptr start_point
    #{poke TSInputEdit, old_end_point} ptr old_end_point
    #{poke TSInputEdit, new_end_point} ptr new_end_point

{-|
  > typedef struct TSNode {
  >   uint32_t context[4];
  >   const void *id;
  >   const TSTree *tree;
  > } TSNode;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSNode" #-}
  TSNode = TSNode
  { _context :: {-# UNPACK #-} !TSNodeContext
  , _id :: {-# UNPACK #-} !(ConstPtr Void)
  , _tree :: {-# UNPACK #-} !(ConstPtr TSTree)
  }

instance Storable TSNode where
  alignment _ = #{alignment TSNode}
  sizeOf _ = #{size TSNode}
  peek ptr = do
    _context <- peekTSNodeContext ( #{ptr TSNode, context} ptr )
    _id <- ConstPtr <$> #{peek TSNode, id} ptr
    _tree <- ConstPtr <$> #{peek TSNode, tree} ptr
    return TSNode{..}
  poke ptr TSNode{..} = do
    pokeTSNodeContext ( #{ptr TSNode, context} ptr ) _context
    #{poke TSNode, id} ptr (unConstPtr _id)
    #{poke TSNode, tree} ptr (unConstPtr _tree)

{-| The type of the @`_context`@ field of a @`TSNode`@ struct.

 > uint32_t context[4];
 -}
data
  TSNodeContext = TSNodeContext
    {-# UNPACK #-} !( #{type uint32_t} )
    {-# UNPACK #-} !( #{type uint32_t} )
    {-# UNPACK #-} !( #{type uint32_t} )
    {-# UNPACK #-} !( #{type uint32_t} )

{-| Peek a @`TSNodeContext`@.

    This does the same as `peek` would, except that @`TSNodeContext`@ is not an instance of `Storable`.
 -}
peekTSNodeContext :: Ptr ( #{type uint32_t} ) -> IO TSNodeContext
peekTSNodeContext ptr = do
  [x0, x1, x2, x3] <- peekArray 4 ptr
  return $ TSNodeContext x0 x1 x2 x3

{-| Poke a @`TSNodeContext`@.

    This does the same as `poke` would, except that @`TSNodeContext`@ is not an instance of `Storable`.
 -}
pokeTSNodeContext :: Ptr ( #{type uint32_t} ) -> TSNodeContext -> IO ()
pokeTSNodeContext ptr (TSNodeContext x0 x1 x2 x3) = do
  pokeArray ptr [x0, x1, x2, x3]

{-|
  > typedef struct TSTreeCursor {
  >   const void *tree;
  >   const void *id;
  >   uint32_t context[3];
  > } TSTreeCursor;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSTreeCursor" #-}
  TSTreeCursor = TSTreeCursor
  { _tree :: {-# UNPACK #-} !(ConstPtr Void)
  , _id :: {-# UNPACK #-} !(ConstPtr Void)
  , _context :: {-# UNPACK #-} !TSTreeCursorContext
  }

instance Storable TSTreeCursor where
  alignment _ = #{alignment TSTreeCursor}
  sizeOf _ = #{size TSTreeCursor}
  peek ptr = do
    _tree <- ConstPtr <$> #{peek TSTreeCursor, tree} ptr
    _id <- ConstPtr <$> #{peek TSTreeCursor, id} ptr
    _context <- peekTSTreeCursorContext ( #{ptr TSTreeCursor, context} ptr )
    return TSTreeCursor{..}
  poke ptr TSTreeCursor{..} = do
    #{poke TSTreeCursor, tree} ptr (unConstPtr _tree)
    #{poke TSTreeCursor, id} ptr (unConstPtr _id)
    pokeTSTreeCursorContext ( #{ptr TSTreeCursor, context} ptr ) _context

{-| The type of the @`_context`@ field of a @`TSTreeCursor`@ struct.

 > uint32_t context[3];
 -}
data
  TSTreeCursorContext = TSTreeCursorContext
    {-# UNPACK #-} !( #{type uint32_t} )
    {-# UNPACK #-} !( #{type uint32_t} )
    {-# UNPACK #-} !( #{type uint32_t} )

{-| Peek a @`TSTreeCursorContext`@.

    This does the same as `peek` would, except that @`TSTreeCursorContext`@ is not an instance of `Storable`.
 -}
peekTSTreeCursorContext :: Ptr ( #{type uint32_t} ) -> IO TSTreeCursorContext
peekTSTreeCursorContext ptr = do
  [x0, x1, x2] <- peekArray 3 ptr
  return $ TSTreeCursorContext x0 x1 x2

{-| Poke a @`TSTreeCursorContext`@.

    This does the same as `poke` would, except that @`TSTreeCursorContext`@ is not an instance of `Storable`.
 -}
pokeTSTreeCursorContext :: Ptr ( #{type uint32_t} ) -> TSTreeCursorContext -> IO ()
pokeTSTreeCursorContext ptr (TSTreeCursorContext x0 x1 x2) = do
  pokeArray ptr [x0, x1, x2]

{-|
  > typedef struct TSQueryCapture {
  >   TSNode node;
  >   uint32_t index;
  > } TSQueryCapture;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSQueryCapture" #-}
  TSQueryCapture = TSQueryCapture
  { _node :: {-# UNPACK #-} !TSNode
  , _index :: {-# UNPACK #-} !( #{type uint32_t} )
  }

instance Storable TSQueryCapture where
  alignment _ = #{alignment TSQueryCapture}
  sizeOf _ = #{size TSQueryCapture}
  peek ptr = do
    _node <- #{peek TSQueryCapture, node} ptr
    _index <- #{peek TSQueryCapture, index} ptr
    return TSQueryCapture{..}
  poke ptr TSQueryCapture{..} = do
    #{poke TSQueryCapture, node} ptr _node
    #{poke TSQueryCapture, index} ptr _index

{-|
  > typedef enum TSQuantifier {
  >   TSQuantifierZero = 0, // must match the array initialization value
  >   TSQuantifierZeroOrOne,
  >   TSQuantifierZeroOrMore,
  >   TSQuantifierOne,
  >   TSQuantifierOneOrMore,
  > } TSQuantifier;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSQuantifier" #-}
  TSQuantifier = TSQuantifier
    { unTSQuantifier :: #{type TSQuantifier}
    }
    deriving (Eq, Show)
    deriving newtype (Storable)

pattern TSQuantifierZero :: TSQuantifier
pattern TSQuantifierZero = TSQuantifier ( #{const TSQuantifierZero} )

pattern TSQuantifierZeroOrOne :: TSQuantifier
pattern TSQuantifierZeroOrOne = TSQuantifier ( #{const TSQuantifierZeroOrOne} )

pattern TSQuantifierZeroOrMore :: TSQuantifier
pattern TSQuantifierZeroOrMore = TSQuantifier ( #{const TSQuantifierZeroOrMore} )

pattern TSQuantifierOne :: TSQuantifier
pattern TSQuantifierOne = TSQuantifier ( #{const TSQuantifierOne} )

pattern TSQuantifierOneOrMore :: TSQuantifier
pattern TSQuantifierOneOrMore = TSQuantifier ( #{const TSQuantifierOneOrMore} )

{-# COMPLETE TSQuantifierZero, TSQuantifierZeroOrOne, TSQuantifierZeroOrMore, TSQuantifierOne, TSQuantifierOneOrMore #-}

{-|
  > typedef struct TSQueryMatch {
  >   uint32_t id;
  >   uint16_t pattern_index;
  >   uint16_t capture_count;
  >   const TSQueryCapture *captures;
  > } TSQueryMatch;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSQueryMatch" #-}
  TSQueryMatch = TSQueryMatch
  { _id :: {-# UNPACK #-} !( #{type uint32_t} )
  , _pattern_index :: {-# UNPACK #-} !( #{type uint16_t} )
  -- , _capture_count :: {-# UNPACK #-} !( #{type uint16_t} )
  -- , _captures :: {-# UNPACK #-} !(Ptr TSQueryCapture)
  , _captures :: ![TSQueryCapture]
  }

instance Storable TSQueryMatch where
  alignment _ = #{alignment TSQueryMatch}
  sizeOf _ = #{size TSQueryMatch}
  peek ptr = do
    _id <- #{peek TSQueryMatch, id} ptr
    _pattern_index <- #{peek TSQueryMatch, pattern_index} ptr
    _capture_count <- #{peek TSQueryMatch, capture_count} ptr
    _captures <- peekTSQueryCapture _capture_count ( #{ptr TSQueryMatch, captures} ptr )
    return TSQueryMatch{..}
  poke ptr TSQueryMatch{..} = do
    #{poke TSQueryMatch, id} ptr _id
    #{poke TSQueryMatch, pattern_index} ptr _pattern_index
    let _capture_count :: ( #{type uint16_t} )
        _capture_count = fromIntegral $ length _captures
    #{poke TSQueryMatch, capture_count} ptr _capture_count
    pokeTSQueryCapture ( #{ptr TSQueryMatch, captures} ptr ) _captures

{-| Peek an array of @`TSQueryCapture`@.

    This does the same as `peekArray`.
 -}
peekTSQueryCapture ::
  ( #{type uint16_t} ) ->
  Ptr TSQueryCapture ->
  IO [TSQueryCapture]
peekTSQueryCapture capture_count ptr =
  peekArray (fromIntegral capture_count) ptr

{-| Poke an array of @`TSQueryCapture`@.

    This does the same as `pokeArray`.
 -}
pokeTSQueryCapture ::
  Ptr TSQueryCapture ->
  [TSQueryCapture] ->
  IO ()
pokeTSQueryCapture ptr captures =
  pokeArray ptr captures

{-|
  > typedef enum TSQueryPredicateStepType {
  >   TSQueryPredicateStepTypeDone,
  >   TSQueryPredicateStepTypeCapture,
  >   TSQueryPredicateStepTypeString,
  > } TSQueryPredicateStepType;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSQueryPredicateStepType" #-}
  TSQueryPredicateStepType = TSQueryPredicateStepType
    { unTSQueryPredicateStepType :: #{type TSQueryPredicateStepType}
    }
    deriving (Eq, Show)
    deriving newtype (Storable)

pattern TSQueryPredicateStepTypeDone :: TSQueryPredicateStepType
pattern TSQueryPredicateStepTypeDone = TSQueryPredicateStepType ( #{const TSQueryPredicateStepTypeDone} )

pattern TSQueryPredicateStepTypeCapture :: TSQueryPredicateStepType
pattern TSQueryPredicateStepTypeCapture = TSQueryPredicateStepType ( #{const TSQueryPredicateStepTypeCapture} )

pattern TSQueryPredicateStepTypeString :: TSQueryPredicateStepType
pattern TSQueryPredicateStepTypeString = TSQueryPredicateStepType ( #{const TSQueryPredicateStepTypeString} )

{-# COMPLETE TSQueryPredicateStepTypeDone, TSQueryPredicateStepTypeCapture, TSQueryPredicateStepTypeString #-}

{-|
 > typedef struct TSQueryPredicateStep {
 >   TSQueryPredicateStepType type;
 >   uint32_t value_id;
 > } TSQueryPredicateStep;
 -}
data
  {-# CTYPE "tree_sitter/api.h" "struct TSQueryPredicateStep" #-}
  TSQueryPredicateStep = TSQueryPredicateStep
    { _type :: {-# UNPACK #-} !TSQueryPredicateStepType
    , _value_id :: {-# UNPACK #-} !( #{type uint32_t} )
    }

instance Storable TSQueryPredicateStep where
  alignment _ = #{alignment TSQueryPredicateStep}
  sizeOf _ = #{size TSQueryPredicateStep}
  peek ptr = do
    _type <- #{peek TSQueryPredicateStep, type} ptr
    _value_id <- #{peek TSQueryPredicateStep, value_id} ptr
    return TSQueryPredicateStep{..}
  poke ptr TSQueryPredicateStep{..} = do
    #{poke TSQueryPredicateStep, type} ptr _type
    #{poke TSQueryPredicateStep, value_id} ptr _value_id

{-|
 > typedef enum TSQueryError {
 >   TSQueryErrorNone = 0,
 >   TSQueryErrorSyntax,
 >   TSQueryErrorNodeType,
 >   TSQueryErrorField,
 >   TSQueryErrorCapture,
 >   TSQueryErrorStructure,
 >   TSQueryErrorLanguage,
 > } TSQueryError;
 -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSQueryError" #-}
  TSQueryError = TSQueryError
    { unTSQueryError :: #{type TSQueryError}
    }
    deriving (Eq, Show)
    deriving newtype (Storable)

pattern TSQueryErrorNone :: TSQueryError
pattern TSQueryErrorNone = TSQueryError ( #{const TSQueryErrorNone} )

pattern TSQueryErrorSyntax :: TSQueryError
pattern TSQueryErrorSyntax = TSQueryError ( #{const TSQueryErrorSyntax} )

pattern TSQueryErrorNodeType :: TSQueryError
pattern TSQueryErrorNodeType = TSQueryError ( #{const TSQueryErrorNodeType} )

pattern TSQueryErrorField :: TSQueryError
pattern TSQueryErrorField = TSQueryError ( #{const TSQueryErrorField} )

pattern TSQueryErrorCapture :: TSQueryError
pattern TSQueryErrorCapture = TSQueryError ( #{const TSQueryErrorCapture} )

pattern TSQueryErrorStructure :: TSQueryError
pattern TSQueryErrorStructure = TSQueryError ( #{const TSQueryErrorStructure} )

pattern TSQueryErrorLanguage :: TSQueryError
pattern TSQueryErrorLanguage = TSQueryError ( #{const TSQueryErrorLanguage} )

{-# COMPLETE TSQueryErrorNone, TSQueryErrorSyntax, TSQueryErrorNodeType, TSQueryErrorField, TSQueryErrorCapture, TSQueryErrorStructure, TSQueryErrorLanguage #-}

{--------------------}
{- Section - Parser -}
{--------------------}

{-|
  Create a new parser.

  > TSParser *ts_parser_new(void);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_new"
  ts_parser_new ::
    IO (Ptr TSParser)

{-|
  Delete the parser, freeing all of the memory that it used.

  > void ts_parser_delete(TSParser *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_delete"
  ts_parser_delete ::
    Ptr TSParser ->
    IO ()

{-| C function pointer to @`ts_parser_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_parser_delete"
  p_ts_parser_delete ::
    FunPtr (
      Ptr TSParser ->
      IO ()
    )

{-|
  Get the parser's current language.

  > const TSLanguage *ts_parser_language(const TSParser *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_language"
  ts_parser_language ::
    ConstPtr TSParser ->
    IO (ConstPtr TSLanguage)

{-|
  Set the language that the parser should use for parsing.

  Returns a boolean indicating whether or not the language was successfully
  assigned. True means assignment succeeded. False means there was a version
  mismatch: the language was generated with an incompatible version of the
  Tree-sitter CLI. Check the language's version using @`ts_language_version`@
  and compare it to this library's @`TREE_SITTER_LANGUAGE_VERSION`@ and
  @`TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION`@ constants.

  > bool ts_parser_set_language(TSParser *self, const TSLanguage *language);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_set_language"
  ts_parser_set_language ::
    Ptr TSParser ->
    ConstPtr TSLanguage ->
    IO CBool

{-|
  Set the ranges of text that the parser should include when parsing.

  By default, the parser will always include entire documents. This function
  allows you to parse only a *portion* of a document but still return a syntax
  tree whose ranges match up with the document as a whole. You can also pass
  multiple disjoint ranges.

  The second and third parameters specify the location and length of an array
  of ranges. The parser does *not* take ownership of these ranges; it copies
  the data, so it doesn't matter how these ranges are allocated.

  If @count@ is zero, then the entire document will be parsed. Otherwise,
  the given ranges must be ordered from earliest to latest in the document,
  and they must not overlap. That is, the following must hold for all:

  > i < count - 1: ranges[i].end_byte <= ranges[i + 1].start_byte

  If this requirement is not satisfied, the operation will fail, the ranges
  will not be assigned, and this function will return @`False`@. On success,
  this function returns @`True`@.

  > bool ts_parser_set_included_ranges(
  >   TSParser *self,
  >   const TSRange *ranges,
  >   uint32_t count
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_set_included_ranges"
  ts_parser_set_included_ranges ::
    Ptr TSParser ->
    ConstPtr TSRange ->
    ( #{type uint32_t} ) ->
    IO CBool

{-|
  Get the ranges of text that the parser will include when parsing.

  The returned pointer is owned by the parser. The caller should not free it
  or write to it. The length of the array will be written to the given
  @count@ pointer.

  > const TSRange *ts_parser_included_ranges(
  >   const TSParser *self,
  >   uint32_t *count
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_included_ranges"
  ts_parser_included_ranges ::
    ConstPtr TSParser ->
    Ptr ( #{type uint32_t} ) ->
    IO (ConstPtr TSRange)

{-|
  Use the parser to parse some source code and create a syntax tree.

  If you are parsing this document for the first time, pass @NULL@ for the
  @old_tree@ parameter. Otherwise, if you have already parsed an earlier
  version of this document and the document has since been edited, pass the
  previous syntax tree so that the unchanged parts of it can be reused.
  This will save time and memory. For this to work correctly, you must have
  already edited the old syntax tree using the @`ts_tree_edit`@ function in a
  way that exactly matches the source code changes.

  The @t`TSInput`@ parameter lets you specify how to read the text. It has the
  following three fields:

  1. @read@: A function to retrieve a chunk of text at a given byte offset
     and (row, column) position. The function should return a pointer to the
     text and write its length to the @bytes_read@ pointer. The parser does
     not take ownership of this buffer; it just borrows it until it has
     finished reading it. The function should write a zero value to the
     @bytes_read@ pointer to indicate the end of the document.
  2. @payload@: An arbitrary pointer that will be passed to each invocation
     of the @read@ function.
  3. @encoding@: An indication of how the text is encoded. Either
     @TSInputEncodingUTF8@ or @TSInputEncodingUTF16@.

  This function returns a syntax tree on success, and @NULL@ on failure. There
  are three possible reasons for failure:

  1. The parser does not have a language assigned. Check for this using the
      @`ts_parser_language`@ function.
  2. Parsing was cancelled due to a timeout that was set by an earlier call to
     the @`ts_parser_set_timeout_micros`@ function. You can resume parsing from
     where the parser left out by calling @`ts_parser_parse`@ again with the
     same arguments. Or you can start parsing from scratch by first calling
     @`ts_parser_reset`@.
  3. Parsing was cancelled using a cancellation flag that was set by an
     earlier call to @`ts_parser_set_cancellation_flag`@. You can resume parsing
     from where the parser left out by calling @`ts_parser_parse`@ again with
     the same arguments.

  > TSTree *ts_parser_parse(
  >   TSParser *self,
  >   const TSTree *old_tree,
  >   TSInput input
  > );
-}
ts_parser_parse ::
  Ptr TSParser ->
  ConstPtr TSTree ->
  TSRead ->
  TSInputEncoding ->
  IO (Ptr TSTree)
ts_parser_parse = \self old_tree readFun encoding ->
  bracket (mkTSReadFunPtr readFun) freeHaskellFunPtr $ \readFun_p ->
    bracket (_wrap_ts_input_new readFun_p encoding) _wrap_ts_input_delete $ \input_p ->
      _wrap_ts_parser_parse self old_tree input_p

foreign import capi safe "TreeSitter/CApi_hsc.h _wrap_ts_parser_parse"
  _wrap_ts_parser_parse ::
    Ptr TSParser ->
    ConstPtr TSTree ->
    Ptr TSInput ->
    IO (Ptr TSTree)

#{def
  TSTree *_wrap_ts_parser_parse(
    TSParser *self,
    const TSTree *old_tree,
    TSInput *input
  )
  {
    return ts_parser_parse(self, old_tree, *input);
  }
}

{-|
  Use the parser to parse some source code stored in one contiguous buffer.
  The first two parameters are the same as in the @`ts_parser_parse`@ function
  above. The second two parameters indicate the location of the buffer and its
  length in bytes.

  > TSTree *ts_parser_parse_string(
  >   TSParser *self,
  >   const TSTree *old_tree,
  >   const char *string,
  >   uint32_t length
  > );
-}
foreign import capi safe "tree_sitter/api.h ts_parser_parse_string"
  ts_parser_parse_string ::
    Ptr TSParser ->
    ConstPtr TSTree ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    IO (Ptr TSTree)

{-|
  Use the parser to parse some source code stored in one contiguous buffer with
  a given encoding. The first four parameters work the same as in the
  @`ts_parser_parse_string`@ method above. The final parameter indicates whether
  the text is encoded as UTF8 or UTF16.

  > TSTree *ts_parser_parse_string_encoding(
  >   TSParser *self,
  >   const TSTree *old_tree,
  >   const char *string,
  >   uint32_t length,
  >   TSInputEncoding encoding
  > );
-}
foreign import capi safe "tree_sitter/api.h ts_parser_parse_string_encoding"
  ts_parser_parse_string_encoding ::
    Ptr TSParser ->
    ConstPtr TSTree ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    TSInputEncoding ->
    IO (Ptr TSTree)

{-|
  Instruct the parser to start the next parse from the beginning.

  If the parser previously failed because of a timeout or a cancellation, then
  by default, it will resume where it left off on the next call to
  @`ts_parser_parse`@ or other parsing functions. If you don't want to resume,
  and instead intend to use this parser to parse some other document, you must
  call @`ts_parser_reset`@ first.

  > void ts_parser_reset(TSParser *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_reset"
  ts_parser_reset ::
    Ptr TSParser ->
    IO ()

{-|
  Set the maximum duration in microseconds that parsing should be allowed to
  take before halting.

  If parsing takes longer than this, it will halt early, returning @NULL@.
  See @`ts_parser_parse`@ for more information.

  > void ts_parser_set_timeout_micros(TSParser *self, uint64_t timeout_micros);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_set_timeout_micros"
  ts_parser_set_timeout_micros ::
    Ptr TSParser ->
    ( #{type uint64_t} ) ->
    IO ()

{-|
  Get the duration in microseconds that parsing is allowed to take.

  > uint64_t ts_parser_timeout_micros(const TSParser *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_timeout_micros"
  ts_parser_timeout_micros ::
    Ptr TSParser ->
    IO ( #{type uint64_t} )

{-|
  Set the parser's current cancellation flag pointer.

  If a non-null pointer is assigned, then the parser will periodically read
  from this pointer during parsing. If it reads a non-zero value, it will
  halt early, returning @NULL@. See @`ts_parser_parse`@ for more information.

  > void ts_parser_set_cancellation_flag(TSParser *self, const size_t *flag);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_set_cancellation_flag"
  ts_parser_set_cancellation_flag ::
    Ptr TSParser ->
    ConstPtr CSize ->
    IO ()

{-|
  Get the parser's current cancellation flag pointer.

  > const size_t *ts_parser_cancellation_flag(const TSParser *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_cancellation_flag"
  ts_parser_cancellation_flag ::
    ConstPtr TSParser ->
    IO (ConstPtr CSize)

{-|
  Set the logger that a parser should use during parsing.

  The parser does not take ownership over the logger payload. If a logger was
  previously assigned, the caller is responsible for releasing any memory
  owned by the previous logger.

  > void ts_parser_set_logger(TSParser *self, TSLogger logger);
-}
ts_parser_set_logger ::
  Ptr TSParser ->
  TSLog ->
  IO ()
ts_parser_set_logger = \self logFun -> mask_ $ do
  logFun_p <- mkTSLogFunPtr logFun
  logger_p <- _wrap_ts_logger_new logFun_p
  _wrap_ts_parser_set_logger self logger_p

#{def
  void _wrap_ts_parser_set_logger(
    TSParser *self,
    TSLogger *logger
  )
  {
    ts_parser_set_logger(self, *logger);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_parser_set_logger"
  _wrap_ts_parser_set_logger ::
    Ptr TSParser ->
    Ptr TSLogger ->
    IO ()

{-|
  Get the parser's current logger.

  > TSLogger ts_parser_logger(const TSParser *self);
-}
ts_parser_logger ::
  ConstPtr TSParser ->
  IO (Maybe TSLog)
ts_parser_logger = \self -> do
  logFun_p <- _wrap_ts_parser_logger self
  pure $
    if logFun_p /= nullFunPtr
      then Just $ unTSLogFunPtr logFun_p
      else Nothing
{-# INLINE ts_parser_logger #-}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_parser_logger"
  _wrap_ts_parser_logger ::
    ConstPtr TSParser ->
    IO (FunPtr TSLog)

#{def
  TSLog _wrap_ts_parser_logger(
    const TSParser *self
  )
  {
    // Get the current logger
    TSLogger logger = ts_parser_logger(self);
    // Copy the payload into the log function
    if(logger.payload) {
      TSLog log;
      memcpy(&log, logger.payload, sizeof log);
      return log;
    }
    return NULL;
  }
}

{-|
  Remove the parser's current logger.
-}
ts_parser_remove_logger ::
  Ptr TSParser ->
  IO (Maybe TSLog)
ts_parser_remove_logger = \self -> mask_ $ do
  logFun_p <- _wrap_ts_parser_remove_logger self
  if logFun_p == nullFunPtr
    then pure Nothing
    else do
      let logFun = unTSLogFunPtr logFun_p
      freeHaskellFunPtr logFun_p
      pure $ Just logFun

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_parser_remove_logger"
  _wrap_ts_parser_remove_logger ::
    Ptr TSParser ->
    IO (FunPtr TSLog)

#{def
  TSLog _wrap_ts_parser_remove_logger(
    TSParser *self
  )
  {
    // Get the current logger
    TSLogger logger = ts_parser_logger(self);
    // Set the parser's logger to NULL
    TSLogger logger_null = {NULL, NULL};
    ts_parser_set_logger(self, logger_null);
    // Copy the payload into the log function
    if (logger.payload) {
      TSLog log;
      memcpy(&log, logger.payload, sizeof log);
      free(logger.payload);
      return log;
    }
    return NULL;
  }
}

{-|
  Set the file descriptor to which the parser should write debugging graphs
  during parsing. The graphs are formatted in the DOT language. You may want
  to pipe these graphs directly to a `dot(1)` process in order to generate
  SVG output. You can turn off this logging by passing a negative number.

  > void ts_parser_print_dot_graphs(TSParser *self, int fd);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_print_dot_graphs"
  ts_parser_print_dot_graphs ::
    Ptr TSParser ->
    ( #{type int} ) ->
    IO ()

{------------------}
{- Section - Tree -}
{------------------}

{-|
  Create a shallow copy of the syntax tree. This is very fast.

  You need to copy a syntax tree in order to use it on more than one thread at
  a time, as syntax trees are not thread safe.

  > TSTree *ts_tree_copy(const TSTree *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_copy"
  ts_tree_copy ::
    Ptr TSTree ->
    IO (Ptr TSTree)

{-|
  Delete the syntax tree, freeing all of the memory that it used.

  > void ts_tree_delete(TSTree *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_delete"
  ts_tree_delete ::
    Ptr TSTree ->
    IO ()

{-| C function pointer to @`ts_tree_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_tree_delete"
  p_ts_tree_delete ::
    FunPtr (
      Ptr TSTree ->
      IO ()
    )

{-|
  Get the root node of the syntax tree.

  > TSNode ts_tree_root_node(const TSTree *self);
-}
ts_tree_root_node ::
  ConstPtr TSTree ->
  IO TSNode
ts_tree_root_node = \self ->
  alloca $ \result_p -> do
    _wrap_ts_tree_root_node self result_p
    peek result_p
{-# INLINE ts_tree_root_node #-}

#{def
  void _wrap_ts_tree_root_node(
    const TSTree *self,
    TSNode *result
  )
  {
    *result = ts_tree_root_node(self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_root_node"
  _wrap_ts_tree_root_node ::
    ConstPtr TSTree ->
    Ptr TSNode ->
    IO ()

{-|
  Get the root node of the syntax tree, but with its position
  shifted forward by the given offset.

  > TSNode ts_tree_root_node_with_offset(
  >   const TSTree *self,
  >   uint32_t offset_bytes,
  >   TSPoint offset_extent
  > );
-}
ts_tree_root_node_with_offset ::
  ConstPtr TSTree ->
  ( #{type uint32_t} ) ->
  TSPoint ->
  IO TSNode
ts_tree_root_node_with_offset = \self offset_bytes offset_extent ->
  alloca $ \result_p -> do
    with offset_extent $ \offset_extent_p ->
      _wrap_ts_tree_root_node_with_offset self offset_bytes offset_extent_p result_p
    peek result_p
{-# INLINE ts_tree_root_node_with_offset #-}

#{def
  void _wrap_ts_tree_root_node_with_offset(
    const TSTree *self,
    uint32_t offset_bytes,
    TSPoint *offset_extent,
    TSNode *result
  )
  {
    *result = ts_tree_root_node_with_offset(self, offset_bytes, *offset_extent);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_root_node_with_offset"
  _wrap_ts_tree_root_node_with_offset ::
    ConstPtr TSTree ->
    ( #{type uint32_t } ) ->
    Ptr TSPoint ->
    Ptr TSNode ->
    IO ()

{-|
  Get the language that was used to parse the syntax tree.

  > const TSLanguage *ts_tree_language(const TSTree *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_language"
  ts_tree_language ::
    Ptr TSTree ->
    IO (ConstPtr TSLanguage)

{-|
  Get the array of included ranges that was used to parse the syntax tree.

  The returned pointer must be freed by the caller.

  > TSRange *ts_tree_included_ranges(const TSTree *self, uint32_t *length);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_included_ranges"
  ts_tree_included_ranges ::
    Ptr TSTree ->
    Ptr ( #{type uint32_t} ) ->
    IO ( Ptr TSRange )

{-|
  Edit the syntax tree to keep it in sync with source code that has been
  edited.

  You must describe the edit both in terms of byte offsets and in terms of
  (row, column) coordinates.

  > void ts_tree_edit(TSTree *self, const TSInputEdit *edit);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_edit"
  ts_tree_edit ::
    Ptr TSTree ->
    Ptr TSInputEdit ->
    IO ()

{-|
  Compare an old edited syntax tree to a new syntax tree representing the same
  document, returning an array of ranges whose syntactic structure has changed.

  For this to work correctly, the old syntax tree must have been edited such
  that its ranges match up to the new tree. Generally, you'll want to call
  this function right after calling one of the @`ts_parser_parse`@ functions.
  You need to pass the old tree that was passed to parse, as well as the new
  tree that was returned from that function.

  The returned array is allocated using @malloc@ and the caller is responsible
  for freeing it using @free@. The length of the array will be written to the
  given @length@ pointer.

  > TSRange *ts_tree_get_changed_ranges(
  >   const TSTree *old_tree,
  >   const TSTree *new_tree,
  >   uint32_t *length
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_get_changed_ranges"
  ts_tree_get_changed_ranges ::
    Ptr TSTree ->
    Ptr TSTree ->
    Ptr ( #{type uint32_t} ) ->
    IO ( Ptr TSRange )

{-|
  Write a DOT graph describing the syntax tree to the given file.

  > void ts_tree_print_dot_graph(const TSTree *self, int file_descriptor);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_print_dot_graph"
  ts_tree_print_dot_graph ::
    Ptr TSTree ->
    ( #{type int} ) ->
    IO ()

{------------------}
{- Section - Node -}
{------------------}

{-|
  Get the node's type as a null-terminated string.

  > const char *ts_node_type(TSNode self);
-}
ts_node_type ::
  TSNode ->
  IO (ConstPtr CChar)
ts_node_type = \self ->
  with self $ \self_p ->
    _wrap_ts_node_type self_p
{-# INLINE ts_node_type #-}

#{def
  const char *_wrap_ts_node_type(TSNode *self)
  {
    return ts_node_type(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_type"
  _wrap_ts_node_type ::
    Ptr TSNode ->
    IO (ConstPtr CChar)

{-|
  Get the node's type as a numerical id.

  > TSSymbol ts_node_symbol(TSNode self);
-}
ts_node_symbol ::
  TSNode ->
  IO TSSymbol
ts_node_symbol = \self ->
  with self $ \self_p ->
    _wrap_ts_node_symbol self_p
{-# INLINE ts_node_symbol #-}

#{def
  TSSymbol _wrap_ts_node_symbol(TSNode *self)
  {
    return ts_node_symbol(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_symbol"
  _wrap_ts_node_symbol ::
    Ptr TSNode ->
    IO TSSymbol

{-|
  Get the node's language.

  > const TSLanguage *ts_node_language(TSNode self);
-}
ts_node_language ::
  TSNode ->
  IO (ConstPtr TSLanguage)
ts_node_language = \self ->
  with self $ \self_p ->
    _wrap_ts_node_language self_p
{-# INLINE ts_node_language #-}

#{def
  const TSLanguage *_wrap_ts_node_language(TSNode *self)
  {
    return ts_node_language(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_language"
  _wrap_ts_node_language ::
    Ptr TSNode ->
    IO (ConstPtr TSLanguage)

{-|
  Get the node's type as it appears in the grammar ignoring aliases as a
  null-terminated string.

  > const char *ts_node_grammar_type(TSNode self);
-}
ts_node_grammar_type ::
  TSNode ->
  IO (ConstPtr CChar)
ts_node_grammar_type = \self ->
  with self $ \self_p ->
    _wrap_ts_node_grammar_type self_p
{-# INLINE ts_node_grammar_type #-}

#{def
  const char *_wrap_ts_node_grammar_type(TSNode *self)
  {
    return ts_node_grammar_type(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_grammar_type"
  _wrap_ts_node_grammar_type ::
    Ptr TSNode ->
    IO (ConstPtr CChar)

{-|
  Get the node's type as a numerical id as it appears in the grammar ignoring
  aliases. This should be used in @`ts_language_next_state`@ instead of
  @`ts_node_symbol`@.

  > TSSymbol ts_node_grammar_symbol(TSNode self);
-}
ts_node_grammar_symbol ::
  TSNode ->
  IO TSSymbol
ts_node_grammar_symbol = \self ->
  with self $ \self_p ->
    _wrap_ts_node_grammar_symbol self_p
{-# INLINE ts_node_grammar_symbol #-}

#{def
  TSSymbol _wrap_ts_node_grammar_symbol(TSNode *self)
  {
    return ts_node_grammar_symbol(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_grammar_symbol"
  _wrap_ts_node_grammar_symbol ::
    Ptr TSNode ->
    IO TSSymbol

{-|
  Get the node's start byte.

  > uint32_t ts_node_start_byte(TSNode self);
-}
ts_node_start_byte ::
  TSNode ->
  IO ( #{type uint32_t} )
ts_node_start_byte = \self ->
  with self $ \self_p ->
    _wrap_ts_node_start_byte self_p
{-# INLINE ts_node_start_byte #-}

#{def
  uint32_t _wrap_ts_node_start_byte(TSNode *self)
  {
    return ts_node_start_byte(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_start_byte"
  _wrap_ts_node_start_byte ::
    Ptr TSNode ->
    IO ( #{type uint32_t} )

{-|
  Get the node's start position in terms of rows and columns.

  > TSPoint ts_node_start_point(TSNode self);
-}
ts_node_start_point ::
  TSNode ->
  IO TSPoint
ts_node_start_point = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_start_point self_p result_p
    peek result_p
{-# INLINE ts_node_start_point #-}

#{def
  void _wrap_ts_node_start_point(TSNode *self, TSPoint *result)
  {
    *result = ts_node_start_point(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_start_point"
  _wrap_ts_node_start_point ::
    Ptr TSNode ->
    Ptr TSPoint ->
    IO ()

{-|
  Get the node's end byte.

  > uint32_t ts_node_end_byte(TSNode self);
-}
ts_node_end_byte ::
  TSNode ->
  IO ( #{type uint32_t} )
ts_node_end_byte = \self ->
  with self $ \self_p ->
    _wrap_ts_node_end_byte self_p
{-# INLINE ts_node_end_byte #-}

#{def
  uint32_t _wrap_ts_node_end_byte(TSNode *self)
  {
    return ts_node_end_byte(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_end_byte"
  _wrap_ts_node_end_byte ::
    Ptr TSNode ->
    IO ( #{type uint32_t} )

{-|
  Get the node's end position in terms of rows and columns.

  > TSPoint ts_node_end_point(TSNode self);
-}
ts_node_end_point ::
  TSNode ->
  IO TSPoint
ts_node_end_point = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_end_point self_p result_p
    peek result_p
{-# INLINE ts_node_end_point #-}

#{def
  void _wrap_ts_node_end_point(TSNode *self, TSPoint *result)
  {
    *result = ts_node_end_point(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_end_point"
  _wrap_ts_node_end_point ::
    Ptr TSNode ->
    Ptr TSPoint ->
    IO ()

{-|
  Get an S-expression representing the node as a string.

  This string is allocated with @malloc@ and the caller is responsible for
  freeing it using @free@.

  > char *ts_node_string(TSNode self);
-}
ts_node_string ::
  TSNode ->
  IO (Ptr CChar)
ts_node_string = \self ->
  with self $ \self_p ->
    _wrap_ts_node_string self_p
{-# INLINE ts_node_string #-}

#{def
  char *_wrap_ts_node_string(TSNode *self)
  {
    return ts_node_string(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_string"
  _wrap_ts_node_string ::
    Ptr TSNode ->
    IO (Ptr CChar)

{-|
  Check if the node is null. Functions like @`ts_node_child`@ and
  @`ts_node_next_sibling`@ will return a null node to indicate that no such node
  was found.

  > bool ts_node_is_null(TSNode self);
-}
ts_node_is_null ::
  TSNode ->
  IO CBool
ts_node_is_null = \self ->
  with self $ \self_p ->
    _wrap_ts_node_is_null self_p
{-# INLINE ts_node_is_null #-}

#{def
  bool _wrap_ts_node_is_null(TSNode *self)
  {
    return ts_node_is_null(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_is_null"
  _wrap_ts_node_is_null ::
    Ptr TSNode ->
    IO CBool

{-|
  Check if the node is *named*. Named nodes correspond to named rules in the
  grammar, whereas *anonymous* nodes correspond to string literals in the
  grammar.

  > bool ts_node_is_named(TSNode self);
-}
ts_node_is_named ::
  TSNode ->
  IO CBool
ts_node_is_named = \self ->
  with self $ \self_p ->
    _wrap_ts_node_is_named self_p
{-# INLINE ts_node_is_named #-}

#{def
  bool _wrap_ts_node_is_named(TSNode *self)
  {
    return ts_node_is_named(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_is_named"
  _wrap_ts_node_is_named ::
    Ptr TSNode ->
    IO CBool

{-|
  Check if the node is *missing*. Missing nodes are inserted by the parser in
  order to recover from certain kinds of syntax errors.

  > bool ts_node_is_missing(TSNode self);
-}
ts_node_is_missing ::
  TSNode ->
  IO CBool
ts_node_is_missing = \self ->
  with self $ \self_p ->
    _wrap_ts_node_is_missing self_p
{-# INLINE ts_node_is_missing #-}

#{def
  bool _wrap_ts_node_is_missing(TSNode *self)
  {
    return ts_node_is_missing(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_is_missing"
  _wrap_ts_node_is_missing ::
    Ptr TSNode ->
    IO CBool

{-|
  Check if the node is *extra*. Extra nodes represent things like comments,
  which are not required the grammar, but can appear anywhere.

  > bool ts_node_is_extra(TSNode self);
-}
ts_node_is_extra ::
  TSNode ->
  IO CBool
ts_node_is_extra = \self ->
  with self $ \self_p ->
    _wrap_ts_node_is_extra self_p
{-# INLINE ts_node_is_extra #-}

#{def
  bool _wrap_ts_node_is_extra(TSNode *self)
  {
    return ts_node_is_extra(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_is_extra"
  _wrap_ts_node_is_extra ::
    Ptr TSNode ->
    IO CBool

{-|
  Check if a syntax node has been edited.

  > bool ts_node_has_changes(TSNode self);
-}
ts_node_has_changes ::
  TSNode ->
  IO CBool
ts_node_has_changes = \self ->
  with self $ \self_p ->
    _wrap_ts_node_has_changes self_p
{-# INLINE ts_node_has_changes #-}

#{def
  bool _wrap_ts_node_has_changes(TSNode *self)
  {
    return ts_node_has_changes(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_has_changes"
  _wrap_ts_node_has_changes ::
    Ptr TSNode ->
    IO CBool

{-|
  Check if the node is a syntax error or contains any syntax errors.

  > bool ts_node_has_error(TSNode self);
-}
ts_node_has_error ::
  TSNode ->
  IO CBool
ts_node_has_error = \self ->
  with self $ \self_p ->
    _wrap_ts_node_has_error self_p
{-# INLINE ts_node_has_error #-}

#{def
  bool _wrap_ts_node_has_error(TSNode *self)
  {
    return ts_node_has_error(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_has_error"
  _wrap_ts_node_has_error ::
    Ptr TSNode ->
    IO CBool

{-|
  Check if the node is a syntax error.

  > bool ts_node_is_error (TSNode self)
-}
ts_node_is_error ::
  TSNode ->
  IO CBool
ts_node_is_error = \self ->
  with self $ \self_p ->
    _wrap_ts_node_is_error self_p
{-# INLINE ts_node_is_error #-}

#{def
  bool _wrap_ts_node_is_error(TSNode *self)
  {
    return ts_node_is_error(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_is_error"
  _wrap_ts_node_is_error ::
    Ptr TSNode ->
    IO CBool

{-|
  Get this node's parse state.

  > TSStateId ts_node_parse_state (TSNode self)
-}
ts_node_parse_state ::
  TSNode ->
  IO TSStateId
ts_node_parse_state = \self ->
  with self $ \self_p ->
    _wrap_ts_node_parse_state self_p
{-# INLINE ts_node_parse_state #-}

#{def
  TSStateId _wrap_ts_node_parse_state(TSNode *self)
  {
    return ts_node_parse_state(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_parse_state"
  _wrap_ts_node_parse_state ::
    Ptr TSNode ->
    IO TSStateId

{-|
  Get the parse state after this node.

  > TSStateId ts_node_next_parse_state (TSNode self)
-}
ts_node_next_parse_state ::
  TSNode ->
  IO TSStateId
ts_node_next_parse_state = \self ->
  with self $ \self_p ->
    _wrap_ts_node_next_parse_state self_p
{-# INLINE ts_node_next_parse_state #-}

#{def
  TSStateId _wrap_ts_node_next_parse_state(TSNode *self)
  {
    return ts_node_next_parse_state(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_next_parse_state"
  _wrap_ts_node_next_parse_state ::
    Ptr TSNode ->
    IO TSStateId

{-|
  Get the node's immediate parent.

  > TSNode ts_node_parent(TSNode self);
-}
ts_node_parent ::
  TSNode ->
  IO TSNode
ts_node_parent = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_parent self_p result_p
    peek result_p
{-# INLINE ts_node_parent #-}

#{def
  void _wrap_ts_node_parent(TSNode *self, TSNode *result)
  {
    *result = ts_node_parent(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_parent"
  _wrap_ts_node_parent ::
    Ptr TSNode ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node that contains @descendant@.

  Note that this can return @descendant@ itself.

  > TSNode ts_node_child_with_descendant(TSNode self, TSNode descendant);
-}
ts_node_child_with_descendant ::
  TSNode ->
  TSNode ->
  IO TSNode
ts_node_child_with_descendant = \self descendant ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      with descendant $ \descendant_p ->
        _wrap_ts_node_child_with_descendant self_p descendant_p result_p
    peek result_p
{-# INLINE ts_node_child_with_descendant #-}

#{def
  void _wrap_ts_node_child_with_descendant(TSNode *self, TSNode *descendant, TSNode *result)
  {
    *result = ts_node_child_with_descendant(*self, *descendant);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_child_with_descendant"
  _wrap_ts_node_child_with_descendant ::
    Ptr TSNode ->
    Ptr TSNode ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's child at the given index, where zero represents the first
  child.

  > TSNode ts_node_child(TSNode self, uint32_t child_index);
-}
ts_node_child ::
  TSNode ->
  ( #{type uint32_t} ) ->
  IO TSNode
ts_node_child = \self child_index ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_child self_p child_index result_p
    peek result_p
{-# INLINE ts_node_child #-}

#{def
  void _wrap_ts_node_child(TSNode *self, uint32_t child_index, TSNode *result)
  {
    *result = ts_node_child(*self, child_index);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_child"
  _wrap_ts_node_child ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the field name for node's child at the given index, where zero represents
  the first child. Returns @NULL@ if no field is found.

  > const char *ts_node_field_name_for_child(TSNode self, uint32_t child_index);
-}
ts_node_field_name_for_child ::
  TSNode ->
  ( #{type uint32_t} ) ->
  IO (ConstPtr CChar)
ts_node_field_name_for_child = \self child_index ->
  with self $ \self_p ->
    _wrap_ts_node_field_name_for_child self_p child_index
{-# INLINE ts_node_field_name_for_child #-}

#{def
  const char *_wrap_ts_node_field_name_for_child(TSNode *self, uint32_t child_index)
  {
    return ts_node_field_name_for_child(*self, child_index);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_field_name_for_child"
  _wrap_ts_node_field_name_for_child ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    IO (ConstPtr CChar)

{-|
  Get the field name for node's named child at the given index, where zero
  represents the first named child. Returns @NULL@, if no field is found.

  > const char *ts_node_field_name_for_named_child(TSNode self, uint32_t named_child_index);
-}
ts_node_field_name_for_named_child ::
  TSNode ->
  ( #{type uint32_t} ) ->
  IO (ConstPtr CChar)
ts_node_field_name_for_named_child = \self child_index ->
  with self $ \self_p ->
    _wrap_ts_node_field_name_for_named_child self_p child_index
{-# INLINE ts_node_field_name_for_named_child #-}

#{def
  const char *_wrap_ts_node_field_name_for_named_child(TSNode *self, uint32_t child_index)
  {
    return ts_node_field_name_for_named_child(*self, child_index);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_field_name_for_named_child"
  _wrap_ts_node_field_name_for_named_child ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    IO (ConstPtr CChar)

{-|
  Get the node's number of children.

  > uint32_t ts_node_child_count(TSNode self);
-}
ts_node_child_count ::
  TSNode ->
  IO ( #{type uint32_t} )
ts_node_child_count = \self ->
  with self $ \self_p ->
    _wrap_ts_node_child_count self_p
{-# INLINE ts_node_child_count #-}

#{def
  uint32_t _wrap_ts_node_child_count(TSNode *self)
  {
    return ts_node_child_count(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_child_count"
  _wrap_ts_node_child_count ::
    Ptr TSNode ->
    IO ( #{type uint32_t} )

{-|
  Get the node's *named* child at the given index.

  See also @`ts_node_is_named`@.

  > TSNode ts_node_named_child(TSNode self, uint32_t child_index);
-}
ts_node_named_child ::
  TSNode ->
  #{type uint32_t} ->
  IO TSNode
ts_node_named_child = \self child_index ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_named_child self_p child_index result_p
    peek result_p
{-# INLINE ts_node_named_child #-}

#{def
  void _wrap_ts_node_named_child(TSNode *self, uint32_t child_index, TSNode *result)
  {
    *result = ts_node_named_child(*self, child_index);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_named_child"
  _wrap_ts_node_named_child ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's number of *named* children.

  See also @`ts_node_is_named`@.

  > uint32_t ts_node_named_child_count(TSNode self);
-}
ts_node_named_child_count ::
  TSNode ->
  IO ( #{type uint32_t} )
ts_node_named_child_count = \self ->
  with self $ \self_p ->
    _wrap_ts_node_named_child_count self_p
{-# INLINE ts_node_named_child_count #-}

#{def
  uint32_t _wrap_ts_node_named_child_count(TSNode *self)
  {
    return ts_node_named_child_count(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_named_child_count"
  _wrap_ts_node_named_child_count ::
    Ptr TSNode ->
    IO ( #{type uint32_t} )

{-|
  Get the node's child with the given field name.

  > TSNode ts_node_child_by_field_name(
  >   TSNode self,
  >   const char *name,
  >   uint32_t name_length
  > );
-}
ts_node_child_by_field_name ::
  TSNode ->
  ConstPtr CChar ->
  ( #{type uint32_t} ) ->
  IO TSNode
ts_node_child_by_field_name = \self name name_length ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_child_by_field_name self_p name name_length result_p
    peek result_p
{-# INLINE ts_node_child_by_field_name #-}

#{def
  void _wrap_ts_node_child_by_field_name(
    TSNode *self,
    const char *name,
    uint32_t name_length,
    TSNode *result
  )
  {
    *result = ts_node_child_by_field_name(*self, name, name_length);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_child_by_field_name"
  _wrap_ts_node_child_by_field_name ::
    Ptr TSNode ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's child with the given numerical field id.

  You can convert a field name to an id using the
  @`ts_language_field_id_for_name`@ function.

  > TSNode ts_node_child_by_field_id(TSNode self, TSFieldId field_id);
-}
ts_node_child_by_field_id ::
  TSNode ->
  TSFieldId ->
  IO TSNode
ts_node_child_by_field_id = \self field_id ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_child_by_field_id self_p field_id result_p
    peek result_p
{-# INLINE ts_node_child_by_field_id #-}

#{def
  void _wrap_ts_node_child_by_field_id(
    TSNode *self,
    TSFieldId field_id,
    TSNode *result
  )
  {
    *result = ts_node_child_by_field_id(*self, field_id);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_child_by_field_id"
  _wrap_ts_node_child_by_field_id ::
    Ptr TSNode ->
    TSFieldId ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's next sibling.

  > TSNode ts_node_next_sibling(TSNode self);
-}
ts_node_next_sibling ::
  TSNode ->
  IO TSNode
ts_node_next_sibling = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_next_sibling self_p result_p
    peek result_p
{-# INLINE ts_node_next_sibling #-}

#{def
  void _wrap_ts_node_next_sibling(TSNode *self, TSNode *result)
  {
    *result = ts_node_next_sibling(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_next_sibling"
  _wrap_ts_node_next_sibling ::
    Ptr TSNode ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's previous sibling.

  > TSNode ts_node_prev_sibling(TSNode self);
-}
ts_node_prev_sibling ::
  TSNode ->
  IO TSNode
ts_node_prev_sibling = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_prev_sibling self_p result_p
    peek result_p
{-# INLINE ts_node_prev_sibling #-}

#{def
  void _wrap_ts_node_prev_sibling(TSNode *self, TSNode *result)
  {
    *result = ts_node_prev_sibling(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_prev_sibling"
  _wrap_ts_node_prev_sibling ::
    Ptr TSNode ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's next *named* sibling.

  > TSNode ts_node_next_named_sibling(TSNode self);
-}
ts_node_next_named_sibling ::
  TSNode ->
  IO TSNode
ts_node_next_named_sibling = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_next_named_sibling self_p result_p
    peek result_p
{-# INLINE ts_node_next_named_sibling #-}

#{def
  void _wrap_ts_node_next_named_sibling(TSNode *self, TSNode *result)
  {
    *result = ts_node_next_named_sibling(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_next_named_sibling"
  _wrap_ts_node_next_named_sibling ::
    Ptr TSNode ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's previous *named* sibling.

  > TSNode ts_node_prev_named_sibling(TSNode self);
-}
ts_node_prev_named_sibling ::
  TSNode ->
  IO TSNode
ts_node_prev_named_sibling = \self ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_prev_named_sibling self_p result_p
    peek result_p
{-# INLINE ts_node_prev_named_sibling #-}

#{def
  void _wrap_ts_node_prev_named_sibling(TSNode *self, TSNode *result)
  {
    *result = ts_node_prev_named_sibling(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_prev_named_sibling"
  _wrap_ts_node_prev_named_sibling ::
    Ptr TSNode ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's first child that extends beyond the given byte offset.

  > TSNode ts_node_first_child_for_byte(TSNode self, uint32_t byte);
-}
ts_node_first_child_for_byte ::
  TSNode ->
  ( #{type uint32_t} ) ->
  IO TSNode
ts_node_first_child_for_byte = \self byte ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_first_child_for_byte self_p byte result_p
    peek result_p
{-# INLINE ts_node_first_child_for_byte #-}

#{def
  void _wrap_ts_node_first_child_for_byte(TSNode *self, uint32_t byte, TSNode *result)
  {
    *result = ts_node_first_child_for_byte(*self, byte);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_first_child_for_byte"
  _wrap_ts_node_first_child_for_byte ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's first named child that extends beyond the given byte offset.

  > TSNode ts_node_first_named_child_for_byte(TSNode self, uint32_t byte);
-}
ts_node_first_named_child_for_byte ::
  TSNode ->
  ( #{type uint32_t} ) ->
  IO TSNode
ts_node_first_named_child_for_byte = \self byte ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_first_named_child_for_byte self_p byte result_p
    peek result_p
{-# INLINE ts_node_first_named_child_for_byte #-}

#{def
  void _wrap_ts_node_first_named_child_for_byte(TSNode *self, uint32_t byte, TSNode *result)
  {
    *result = ts_node_first_named_child_for_byte(*self, byte);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_first_named_child_for_byte"
  _wrap_ts_node_first_named_child_for_byte ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the node's number of descendants, including one for the node itself.

  > uint32_t ts_node_descendant_count(TSNode self);
-}
ts_node_descendant_count ::
  TSNode ->
  IO ( #{type uint32_t} )
ts_node_descendant_count = \self ->
  with self $ \self_p ->
    _wrap_ts_node_descendant_count self_p
{-# INLINE ts_node_descendant_count #-}

#{def
  uint32_t _wrap_ts_node_descendant_count(TSNode *self)
  {
    return ts_node_descendant_count(*self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_descendant_count"
  _wrap_ts_node_descendant_count ::
    Ptr TSNode ->
    IO ( #{type uint32_t} )

{-|
  Get the smallest node within this node that spans the given range of bytes.

  > TSNode ts_node_descendant_for_byte_range(TSNode self, uint32_t start, uint32_t end);
-}
ts_node_descendant_for_byte_range ::
  TSNode ->
  ( #{type uint32_t} ) ->
  ( #{type uint32_t} ) ->
  IO TSNode
ts_node_descendant_for_byte_range = \self start end ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_descendant_for_byte_range self_p start end result_p
    peek result_p
{-# INLINE ts_node_descendant_for_byte_range #-}

#{def
  void _wrap_ts_node_descendant_for_byte_range(TSNode *self, uint32_t start, uint32_t end, TSNode *result)
  {
    *result = ts_node_descendant_for_byte_range(*self, start, end);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_descendant_for_byte_range"
  _wrap_ts_node_descendant_for_byte_range ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the smallest node within this node that spans the given range of positions.

  > TSNode ts_node_descendant_for_point_range(TSNode self, TSPoint start, TSPoint end);
-}
ts_node_descendant_for_point_range ::
  TSNode ->
  TSPoint ->
  TSPoint ->
  IO TSNode
ts_node_descendant_for_point_range = \self start end ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      with start $ \start_p ->
        with end $ \end_p ->
          _wrap_ts_node_descendant_for_point_range self_p start_p end_p result_p
    peek result_p
{-# INLINE ts_node_descendant_for_point_range #-}

#{def
  void _wrap_ts_node_descendant_for_point_range(TSNode *self, TSPoint *start, TSPoint *end, TSNode *result)
  {
    *result = ts_node_descendant_for_point_range(*self, *start, *end);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_descendant_for_point_range"
  _wrap_ts_node_descendant_for_point_range ::
    Ptr TSNode ->
    Ptr TSPoint ->
    Ptr TSPoint ->
    Ptr TSNode ->
    IO ()

{-|
  Get the smallest named node within this node that spans the given range of
  bytes.

  > TSNode ts_node_named_descendant_for_byte_range(TSNode self, uint32_t start, uint32_t end);
-}
ts_node_named_descendant_for_byte_range ::
  TSNode ->
  ( #{type uint32_t} ) ->
  ( #{type uint32_t} ) ->
  IO TSNode
ts_node_named_descendant_for_byte_range = \self start end ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      _wrap_ts_node_named_descendant_for_byte_range self_p start end result_p
    peek result_p
{-# INLINE ts_node_named_descendant_for_byte_range #-}

#{def
  void _wrap_ts_node_named_descendant_for_byte_range(TSNode *self, uint32_t start, uint32_t end, TSNode *result)
  {
    *result = ts_node_named_descendant_for_byte_range(*self, start, end);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_named_descendant_for_byte_range"
  _wrap_ts_node_named_descendant_for_byte_range ::
    Ptr TSNode ->
    ( #{type uint32_t} ) ->
    ( #{type uint32_t} ) ->
    Ptr TSNode ->
    IO ()

{-|
  Get the smallest named node within this node that spans the given range of positions.

  > TSNode ts_node_named_descendant_for_point_range(TSNode self, TSPoint start, TSPoint end);
-}
ts_node_named_descendant_for_point_range ::
  TSNode ->
  TSPoint ->
  TSPoint ->
  IO TSNode
ts_node_named_descendant_for_point_range = \self start end ->
  alloca $ \result_p -> do
    with self $ \self_p ->
      with start $ \start_p ->
        with end $ \end_p ->
          _wrap_ts_node_named_descendant_for_point_range self_p start_p end_p result_p
    peek result_p
{-# INLINE ts_node_named_descendant_for_point_range #-}

#{def
  void _wrap_ts_node_named_descendant_for_point_range(TSNode *self, TSPoint *start, TSPoint *end, TSNode *result)
  {
    *result = ts_node_named_descendant_for_point_range(*self, *start, *end);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_named_descendant_for_point_range"
  _wrap_ts_node_named_descendant_for_point_range ::
    Ptr TSNode ->
    Ptr TSPoint ->
    Ptr TSPoint ->
    Ptr TSNode ->
    IO ()

{-|
  Edit the node to keep it in-sync with source code that has been edited.

  This function is only rarely needed. When you edit a syntax tree with the
  @`ts_tree_edit`@ function, all of the nodes that you retrieve from the tree
  afterward will already reflect the edit. You only need to use @`ts_node_edit`@
  when you have a @t`TSNode`@ instance that you want to keep and continue to use
  after an edit.

  > void ts_node_edit(TSNode *self, const TSInputEdit *edit);
-}
foreign import capi unsafe "TreeSitter/CApi_hsc.h ts_node_edit"
  ts_node_edit ::
    Ptr TSNode ->
    ConstPtr TSInputEdit ->
    IO ()

{-|
  Check if two nodes are identical.

  > bool ts_node_eq(TSNode self, TSNode other);
-}
ts_node_eq ::
  TSNode ->
  TSNode ->
  IO CBool
ts_node_eq = \self other ->
  with self $ \self_p ->
    with other $ \other_p ->
        _wrap_ts_node_eq self_p other_p
{-# INLINE ts_node_eq #-}

#{def
  bool _wrap_ts_node_eq(TSNode *self, TSNode *other)
  {
    return ts_node_eq(*self, *other);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_node_eq"
  _wrap_ts_node_eq ::
    Ptr TSNode ->
    Ptr TSNode ->
    IO CBool

{- Section - TreeCursor -}

{-|
  Create a new tree cursor starting from the given node.

  A tree cursor allows you to walk a syntax tree more efficiently than is
  possible using the @t`TSNode`@ functions. It is a mutable object that is always
  on a certain syntax node, and can be moved imperatively to different nodes.

  > TSTreeCursor ts_tree_cursor_new(TSNode node);
-}
ts_tree_cursor_new ::
  TSNode ->
  IO TSTreeCursor
ts_tree_cursor_new = \node ->
  alloca $ \result_p -> do
    ts_tree_cursor_new_p node result_p
    peek result_p
{-# INLINE ts_tree_cursor_new #-}

-- | Create a new tree cursor starting from the given node.
--
--   Variant of 'ts_tree_cursor_new' that writes the tree cursor to the provided pointer.
ts_tree_cursor_new_p ::
  TSNode ->
  Ptr TSTreeCursor -> -- ^ Output pointer for the new tree cursor.
  IO ()
ts_tree_cursor_new_p = \node result_p ->
  with node $ \node_p ->
    _wrap_ts_tree_cursor_new node_p result_p
{-# INLINE ts_tree_cursor_new_p #-}

#{def
  void _wrap_ts_tree_cursor_new(TSNode *node, TSTreeCursor *result)
  {
    *result = ts_tree_cursor_new(*node);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_cursor_new"
  _wrap_ts_tree_cursor_new ::
    Ptr TSNode ->
    Ptr TSTreeCursor ->
    IO ()

{-|
  Delete a tree cursor, freeing all of the memory that it used.

  > void ts_tree_cursor_delete(TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_delete"
  ts_tree_cursor_delete ::
    Ptr TSTreeCursor ->
    IO ()

{-| C function pointer to @`ts_tree_cursor_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_tree_cursor_delete"
  p_ts_tree_cursor_delete ::
    FunPtr (
      Ptr TSTreeCursor ->
      IO ()
    )

{-|
  Re-initialize a tree cursor to start at the original node that the cursor was
  constructed with.

  > void ts_tree_cursor_reset(TSTreeCursor *self, TSNode node);
-}
ts_tree_cursor_reset ::
  Ptr TSTreeCursor ->
  TSNode ->
  IO ()
ts_tree_cursor_reset = \self node ->
  with node $ \node_p ->
    _wrap_ts_tree_cursor_reset self node_p
{-# INLINE ts_tree_cursor_reset #-}

#{def
  void _wrap_ts_tree_cursor_reset(TSTreeCursor *self, TSNode *node)
  {
    ts_tree_cursor_reset(self, *node);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_cursor_reset"
  _wrap_ts_tree_cursor_reset ::
    Ptr TSTreeCursor ->
    Ptr TSNode ->
    IO ()

{-|
  Re-initialize a tree cursor to the same position as another cursor.

  Unlike @`ts_tree_cursor_reset`@, this will not lose parent information and
  allows reusing already created cursors.

  > void ts_tree_cursor_reset_to(TSTreeCursor *dst, const TSTreeCursor *src);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_reset_to"
  ts_tree_cursor_reset_to ::
    Ptr TSTreeCursor ->
    ConstPtr TSTreeCursor ->
    IO ()

{-|
  Get the tree cursor's current node.

  > TSNode ts_tree_cursor_current_node(const TSTreeCursor *self);
-}
ts_tree_cursor_current_node ::
  ConstPtr TSTreeCursor ->
  IO TSNode
ts_tree_cursor_current_node = \self ->
  alloca $ \result_p -> do
    _wrap_ts_tree_cursor_current_node self result_p
    peek result_p
{-# INLINE ts_tree_cursor_current_node #-}

#{def
  void _wrap_ts_tree_cursor_current_node(const TSTreeCursor *self, TSNode *result)
  {
    *result = ts_tree_cursor_current_node(self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_cursor_current_node"
  _wrap_ts_tree_cursor_current_node ::
    ConstPtr TSTreeCursor ->
    Ptr TSNode ->
    IO ()

{-|
  Get the field name of the tree cursor's current node.

  This returns @NULL@ if the current node doesn't have a field.
  See also @`ts_node_child_by_field_name`@.

  > const char *ts_tree_cursor_current_field_name(const TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_current_field_name"
  ts_tree_cursor_current_field_name ::
    ConstPtr TSTreeCursor ->
    IO (ConstPtr CChar)

{-|
  Get the field id of the tree cursor's current node.

  This returns zero if the current node doesn't have a field.
  See also @`ts_node_child_by_field_id`@, @`ts_language_field_id_for_name`@.

  > TSFieldId ts_tree_cursor_current_field_id(const TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_current_field_id"
  ts_tree_cursor_current_field_id ::
    ConstPtr TSTreeCursor ->
    IO TSFieldId

{-|
  Move the cursor to the parent of its current node.

  This returns @true@ if the cursor successfully moved, and returns @false@
  if there was no parent node (the cursor was already on the root node).

  > bool ts_tree_cursor_goto_parent(TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_parent"
  ts_tree_cursor_goto_parent ::
    Ptr TSTreeCursor ->
    IO CBool

{-|
  Move the cursor to the next sibling of its current node.

  This returns @true@ if the cursor successfully moved, and returns @false@
  if there was no next sibling node.

  > bool ts_tree_cursor_goto_next_sibling(TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_next_sibling"
  ts_tree_cursor_goto_next_sibling ::
    Ptr TSTreeCursor ->
    IO CBool

{-|
  Move the cursor to the previous sibling of its current node.

  This returns @true@ if the cursor successfully moved, and returns @false@ if
  there was no previous sibling node.

  Note, that this function may be slower than
  @`ts_tree_cursor_goto_next_sibling`@ due to how node positions are stored. In
  the worst case, this will need to iterate through all the children upto the
  previous sibling node to recalculate its position.

  > bool ts_tree_cursor_goto_previous_sibling(TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_previous_sibling"
  ts_tree_cursor_goto_previous_sibling ::
    Ptr TSTreeCursor ->
    IO CBool

{-|
  Move the cursor to the first child of its current node.

  This returns @true@ if the cursor successfully moved, and returns @false@
  if there were no children.

  > bool ts_tree_cursor_goto_first_child(TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_first_child"
  ts_tree_cursor_goto_first_child ::
    Ptr TSTreeCursor ->
    IO CBool

{-|
  Move the cursor to the last child of its current node.

  This returns @true@ if the cursor successfully moved, and returns @false@ if
  there were no children.

  Note that this function may be slower than @`ts_tree_cursor_goto_first_child`@
  because it needs to iterate through all the children to compute the child's
  position.

  > bool ts_tree_cursor_goto_last_child(TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_last_child"
  ts_tree_cursor_goto_last_child ::
    Ptr TSTreeCursor ->
    IO CBool

{-|
  Move the cursor to the node that is the nth descendant of
  the original node that the cursor was constructed with, where
  zero represents the original node itself.

  > void ts_tree_cursor_goto_descendant(TSTreeCursor *self, uint32_t goal_descendant_index);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_descendant"
  ts_tree_cursor_goto_descendant ::
    Ptr TSTreeCursor ->
    ( #{type uint32_t} ) ->
    IO ()

{-|
  Get the index of the cursor's current node out of all of the
  descendants of the original node that the cursor was constructed with.

  > uint32_t ts_tree_cursor_current_descendant_index(const TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_current_descendant_index"
  ts_tree_cursor_current_descendant_index ::
    ConstPtr TSTreeCursor ->
    IO ( #{type uint32_t} )

{-|
  Get the depth of the cursor's current node relative to the original
  node that the cursor was constructed with.

  > uint32_t ts_tree_cursor_current_depth(const TSTreeCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_current_depth"
  ts_tree_cursor_current_depth ::
    ConstPtr TSTreeCursor ->
    IO ( #{type uint32_t} )

{-|
  Move the cursor to the first child of its current node that extends beyond
  the given byte offset.

  This returns the index of the child node if one was found, and returns -1
  if no such child was found.

  > int64_t ts_tree_cursor_goto_first_child_for_byte(TSTreeCursor *self, uint32_t goal_byte);
-}
foreign import capi unsafe "tree_sitter/api.h ts_tree_cursor_goto_first_child_for_byte"
  ts_tree_cursor_goto_first_child_for_byte ::
    Ptr TSTreeCursor ->
    ( #{type uint32_t} ) ->
    IO ( #{type int64_t} )

{-|
  Move the cursor to the first child of its current node that extends beyond
  the given byte point.

  This returns the index of the child node if one was found, and returns -1
  if no such child was found.

  > int64_t ts_tree_cursor_goto_first_child_for_point(TSTreeCursor *self, TSPoint goal_point);
-}
ts_tree_cursor_goto_first_child_for_point ::
    Ptr TSTreeCursor ->
    TSPoint ->
    IO ( #{type int64_t} )
ts_tree_cursor_goto_first_child_for_point = \self goal_point ->
  with goal_point $ \goal_point_p ->
    _wrap_ts_tree_cursor_goto_first_child_for_point self goal_point_p
{-# INLINE ts_tree_cursor_goto_first_child_for_point #-}

#{def
  int64_t _wrap_ts_tree_cursor_goto_first_child_for_point(TSTreeCursor *self, TSPoint *goal_point)
  {
    return ts_tree_cursor_goto_first_child_for_point(self, *goal_point);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_cursor_goto_first_child_for_point"
  _wrap_ts_tree_cursor_goto_first_child_for_point ::
    Ptr TSTreeCursor ->
    Ptr TSPoint ->
    IO ( #{type int64_t} )

{-|
  > TSTreeCursor ts_tree_cursor_copy(const TSTreeCursor *cursor);
  -}
ts_tree_cursor_copy ::
    Ptr TSTreeCursor ->
    IO TSTreeCursor
ts_tree_cursor_copy = \self ->
  alloca $ \result_p -> do
    _wrap_ts_tree_cursor_copy self result_p
    peek result_p
{-# INLINE ts_tree_cursor_copy #-}

-- | Copy a tree cursor.
--
--   Variant of 'ts_tree_cursor_copy' that writes the tree cursor to the provided pointer.
ts_tree_cursor_copy_p ::
  Ptr TSTreeCursor ->
  Ptr TSTreeCursor -> -- ^ Output pointer for the new tree cursor.
  IO ()
ts_tree_cursor_copy_p = _wrap_ts_tree_cursor_copy
{-# INLINE ts_tree_cursor_copy_p #-}


#{def
  void _wrap_ts_tree_cursor_copy(TSTreeCursor *self, TSTreeCursor *result)
  {
    *result = ts_tree_cursor_copy(self);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_tree_cursor_copy"
  _wrap_ts_tree_cursor_copy ::
    Ptr TSTreeCursor ->
    Ptr TSTreeCursor ->
    IO ()

{- Section - Query -}

{-|
  Create a new query from a string containing one or more S-expression
  patterns. The query is associated with a particular language, and can
  only be run on syntax nodes parsed with that language.

  If all of the given patterns are valid, this returns a @t`TSQuery`@.
  If a pattern is invalid, this returns @NULL@, and provides two pieces
  of information about the problem:
  1. The byte offset of the error is written to the @error_offset@ parameter.
  2. The type of error is written to the @error_type@ parameter.

  > TSQuery *ts_query_new(
  >   const TSLanguage *language,
  >   const char *source,
  >   uint32_t source_len,
  >   uint32_t *error_offset,
  >   TSQueryError *error_type
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_new"
  ts_query_new ::
    ConstPtr TSLanguage ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    Ptr ( #{type uint32_t} ) ->
    Ptr TSQueryError ->
    IO (Ptr TSQuery)

{-|
  Delete a query, freeing all of the memory that it used.

  > void ts_query_delete(TSQuery *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_delete"
  ts_query_delete ::
    Ptr TSQuery ->
    IO ()

{-| C function pointer to @`ts_query_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_query_delete"
  p_ts_query_delete ::
    FunPtr (
      Ptr TSQuery ->
      IO ()
    )

{-|
  Get the number of patterns in the query.

  > uint32_t ts_query_pattern_count(const TSQuery *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_pattern_count"
  ts_query_pattern_count ::
    ConstPtr TSQuery ->
    IO ( #{type uint32_t} )

{-|
  Get the number of captures in the query.

  > uint32_t ts_query_capture_count(const TSQuery *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_capture_count"
  ts_query_capture_count ::
    ConstPtr TSQuery ->
    IO ( #{type uint32_t} )

{-|
  Get the number of string literals in the query.

  > uint32_t ts_query_string_count(const TSQuery *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_string_count"
  ts_query_string_count ::
    ConstPtr TSQuery ->
    IO ( #{type uint32_t} )

{-|
  Get the byte offset where the given pattern starts in the query's source.

  This can be useful when combining queries by concatenating their source
  code strings.

  > uint32_t ts_query_start_byte_for_pattern(const TSQuery *self, uint32_t pattern_index);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_start_byte_for_pattern"
  ts_query_start_byte_for_pattern ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    IO ( #{type uint32_t} )

{-|
  Get the byte offset where the given pattern ends in the query's source.

  This can be useful when combining queries by concatenating their source
  code strings.

  > uint32_t ts_query_end_byte_for_pattern(const TSQuery *self, uint32_t pattern_index);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_end_byte_for_pattern"
  ts_query_end_byte_for_pattern ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    IO ( #{type uint32_t} )

{-|
  Get all of the predicates for the given pattern in the query.

  The predicates are represented as a single array of steps. There are three
  types of steps in this array, which correspond to the three legal values for
  the `type` field:
  - @`TSQueryPredicateStepTypeCapture`@ - Steps with this type represent names
     of captures. Their @value_id@ can be used with the
    @`ts_query_capture_name_for_id`@ function to obtain the name of the capture.
  - @`TSQueryPredicateStepTypeString`@ - Steps with this type represent literal
     strings. Their @value_id@ can be used with the
     @`ts_query_string_value_for_id`@ function to obtain their string value.
  - @`TSQueryPredicateStepTypeDone`@ - Steps with this type are *sentinels*
     that represent the end of an individual predicate. If a pattern has two
     predicates, then there will be two steps with this `type` in the array.

  > const TSQueryPredicateStep *ts_query_predicates_for_pattern(
  >   const TSQuery *self,
  >   uint32_t pattern_index,
  >   uint32_t *step_count
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_predicates_for_pattern"
  ts_query_predicates_for_pattern ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    Ptr ( #{type uint32_t} ) ->
    IO (ConstPtr TSQueryPredicateStep)

{-|
  Check if the given pattern in the query has a single root node.

  > bool ts_query_is_pattern_rooted(const TSQuery *self, uint32_t pattern_index);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_is_pattern_rooted"
  ts_query_is_pattern_rooted ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    IO CBool

{-|
  Check if the given pattern in the query is non-local.

  A non-local pattern has multiple root nodes and can match within a
  repeating sequence of nodes, as specified by the grammar. Non-local
  patterns disable certain optimizations that would otherwise be possible
  when executing a query on a specific range of a syntax tree.

  > bool ts_query_is_pattern_non_local(const TSQuery *self, uint32_t pattern_index);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_is_pattern_non_local"
  ts_query_is_pattern_non_local ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    IO CBool

{-|
  Check if a given pattern is guaranteed to match once a given step is reached.
  The step is specified by its byte offset in the query's source code.

  > bool ts_query_is_pattern_guaranteed_at_step(const TSQuery *self, uint32_t byte_offset);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_is_pattern_guaranteed_at_step"
  ts_query_is_pattern_guaranteed_at_step ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    IO CBool

{-|
  Get the name and length of one of the query's captures, or one of the
  query's string literals. Each capture and string is associated with a
  numeric id based on the order that it appeared in the query's source.

  > const char *ts_query_capture_name_for_id(
  >   const TSQuery *self,
  >   uint32_t index,
  >   uint32_t *length
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_capture_name_for_id"
  ts_query_capture_name_for_id ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    Ptr ( #{type uint32_t} ) ->
    IO (ConstPtr CChar)

{-|
  Get the quantifier of the query's captures. Each capture is * associated
  with a numeric id based on the order that it appeared in the query's source.

  > TSQuantifier ts_query_capture_quantifier_for_id(
  >   const TSQuery *self,
  >   uint32_t pattern_index,
  >   uint32_t capture_index
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_capture_quantifier_for_id"
  ts_query_capture_quantifier_for_id ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    ( #{type uint32_t} ) ->
    IO TSQuantifier

{-|
  > const char *ts_query_string_value_for_id(
  >   const TSQuery *self,
  >   uint32_t index,
  >   uint32_t *length
  > );
  -}
foreign import capi unsafe "tree_sitter/api.h ts_query_string_value_for_id"
  ts_query_string_value_for_id ::
    ConstPtr TSQuery ->
    ( #{type uint32_t} ) ->
    Ptr ( #{type uint32_t} ) ->
    IO (ConstPtr CChar)

{-|
  Disable a certain capture within a query.

  This prevents the capture from being returned in matches, and also avoids
  any resource usage associated with recording the capture. Currently, there
  is no way to undo this.

  > void ts_query_disable_capture(TSQuery *self, const char *name, uint32_t length);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_disable_capture"
  ts_query_disable_capture ::
    Ptr TSQuery ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    IO ()

{-|
  Disable a certain pattern within a query.

  This prevents the pattern from matching and removes most of the overhead
  associated with the pattern. Currently, there is no way to undo this.

  > void ts_query_disable_pattern(TSQuery *self, uint32_t pattern_index);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_disable_pattern"
  ts_query_disable_pattern ::
    Ptr TSQuery ->
    ( #{type uint32_t} ) ->
    IO ()

{-|
  Create a new cursor for executing a given query.

  The cursor stores the state that is needed to iteratively search
  for matches. To use the query cursor, first call @`ts_query_cursor_exec`@
  to start running a given query on a given syntax node. Then, there are
  two options for consuming the results of the query:
  1. Repeatedly call @`ts_query_cursor_next_match`@ to iterate over all of the
     *matches* in the order that they were found. Each match contains the
     index of the pattern that matched, and an array of captures. Because
     multiple patterns can match the same set of nodes, one match may contain
     captures that appear *before* some of the captures from a previous match.
  2. Repeatedly call @`ts_query_cursor_next_capture`@ to iterate over all of the
     individual *captures* in the order that they appear. This is useful if
     don't care about which pattern matched, and just want a single ordered
     sequence of captures.

  If you don't care about consuming all of the results, you can stop calling
  @`ts_query_cursor_next_match`@ or @`ts_query_cursor_next_capture`@ at any point.
   You can then start executing another query on another node by calling
   @`ts_query_cursor_exec`@ again.

    > TSQueryCursor *ts_query_cursor_new(void);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_new"
  ts_query_cursor_new ::
    IO (Ptr TSQueryCursor)

{-|
  Delete a query cursor, freeing all of the memory that it used.

  > void ts_query_cursor_delete(TSQueryCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_delete"
  ts_query_cursor_delete ::
    Ptr TSQueryCursor ->
    IO ()

{-| C function pointer to @`ts_query_cursor_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_query_cursor_delete"
  p_ts_query_cursor_delete ::
    FunPtr (
      Ptr TSQueryCursor ->
      IO ()
    )

{-|
  Start running a given query on a given node.

  > void ts_query_cursor_exec(TSQueryCursor *self, const TSQuery *query, TSNode node);
-}
ts_query_cursor_exec ::
  Ptr TSQueryCursor ->
  ConstPtr TSQuery ->
  TSNode ->
  IO ()
ts_query_cursor_exec = \self query node ->
  with node $ \node_p ->
    _wrap_ts_query_cursor_exec self query node_p
{-# INLINE ts_query_cursor_exec #-}

#{def
  void _wrap_ts_query_cursor_exec(TSQueryCursor *self, const TSQuery *query, TSNode *node)
  {
    return ts_query_cursor_exec(self, query, *node);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_query_cursor_exec"
  _wrap_ts_query_cursor_exec ::
    Ptr TSQueryCursor ->
    ConstPtr TSQuery ->
    Ptr TSNode ->
    IO ()

{-|
  Check whether the maximum number of in-progress matches allowed by this query cursor was exceeded.

  Query cursors have an optional maximum capacity for storing lists of
  in-progress captures. If this capacity is exceeded, then the
  earliest-starting match will silently be dropped to make room for further
  matches. This maximum capacity is optional — by default, query cursors allow
  any number of pending matches, dynamically allocating new space for them as
  needed as the query is executed.

  > bool ts_query_cursor_did_exceed_match_limit(const TSQueryCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_did_exceed_match_limit"
  ts_query_cursor_did_exceed_match_limit ::
    ConstPtr TSQueryCursor ->
    IO CBool

{-|
  Get the maximum number of in-progress matches allowed by this query cursor.

  > uint32_t ts_query_cursor_match_limit(const TSQueryCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_match_limit"
  ts_query_cursor_match_limit ::
    ConstPtr TSQueryCursor ->
    IO ( #{type uint32_t} )

{-|
  Set the maximum number of in-progress matches allowed by this query cursor.

  > void ts_query_cursor_set_match_limit(TSQueryCursor *self, uint32_t limit);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_set_match_limit"
  ts_query_cursor_set_match_limit ::
    Ptr TSQueryCursor ->
    ( #{type uint32_t} ) ->
    IO ()

{-|
  Set the maximum duration in microseconds that query execution should be allowed to
  take before halting.

  If query execution takes longer than this, it will halt early, returning @NULL@.
  See @`ts_query_cursor_next_match`@ or @`ts_query_cursor_next_capture`@ for more information.

  > void ts_query_cursor_set_timeout_micros(TSQueryCursor *self, uint64_t timeout_micros);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_set_timeout_micros"
  ts_query_cursor_set_timeout_micros ::
    Ptr TSQueryCursor ->
    ( #{type uint64_t} ) ->
    IO ()

{-|
  Get the duration in microseconds that query execution is allowed to take.

  This is set via @`ts_query_cursor_set_timeout_micros`@.

  > uint64_t ts_query_cursor_timeout_micros(const TSQueryCursor *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_timeout_micros"
  ts_query_cursor_timeout_micros ::
    ConstPtr TSQueryCursor ->
    IO ( #{type uint64_t} )

{-|
  Set the range of bytes in which the query will be executed.

  > void ts_query_cursor_set_byte_range(TSQueryCursor *self, uint32_t start_byte, uint32_t end_byte);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_set_byte_range"
  ts_query_cursor_set_byte_range ::
    Ptr TSQueryCursor ->
    ( #{type uint32_t} ) ->
    ( #{type uint32_t} ) ->
    IO ()

{-|
  Set the range of positions in which the query will be executed.

  > void ts_query_cursor_set_point_range(TSQueryCursor *self, TSPoint start_point, TSPoint end_point);
-}
ts_query_cursor_set_point_range ::
    Ptr TSQueryCursor ->
    TSPoint ->
    TSPoint ->
    IO ()
ts_query_cursor_set_point_range = \self start_point end_point ->
  with start_point $ \start_point_p ->
    with end_point $ \end_point_p ->
      _wrap_ts_query_cursor_set_point_range self start_point_p end_point_p
{-# INLINE ts_query_cursor_set_point_range #-}

#{def
  void _wrap_ts_query_cursor_set_point_range(TSQueryCursor *self, TSPoint *start_point, TSPoint *end_point)
  {
    ts_query_cursor_set_point_range(self, *start_point, *end_point);
  }
}

foreign import capi unsafe "TreeSitter/CApi_hsc.h _wrap_ts_query_cursor_set_point_range"
  _wrap_ts_query_cursor_set_point_range ::
    Ptr TSQueryCursor ->
    Ptr TSPoint ->
    Ptr TSPoint ->
    IO ()

{-|
  Advance to the next match of the currently running query.

  If there is a match, write it to `*match` and return @true@.
  Otherwise, return @false@.

  > bool ts_query_cursor_next_match(TSQueryCursor *self, TSQueryMatch *match);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_next_match"
  ts_query_cursor_next_match ::
    Ptr TSQueryCursor ->
    Ptr TSQueryMatch ->
    IO CBool

{-|
  Remove a match of the currently running query.

  > void ts_query_cursor_remove_match(TSQueryCursor *self, uint32_t match_id);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_remove_match"
  ts_query_cursor_remove_match ::
    Ptr TSQueryCursor ->
    ( #{type uint32_t} ) ->
    IO ()

{-|
  Advance to the next capture of the currently running query.

  If there is a capture, write its match to `*match` and its index within
  the matche's capture list to `*capture_index`. Otherwise, return @false@.

  > bool ts_query_cursor_next_capture(
  >   TSQueryCursor *self,
  >   TSQueryMatch *match,
  >   uint32_t *capture_index
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_next_capture"
  ts_query_cursor_next_capture ::
    Ptr TSQueryCursor ->
    Ptr TSQueryMatch ->
    Ptr ( #{type uint32_t} ) ->
    IO CBool

{-|
  Set the maximum start depth for a query cursor.

  This prevents cursors from exploring children nodes at a certain depth.
  Note if a pattern includes many children, then they will still be checked.

  The zero max start depth value can be used as a special behavior and
  it helps to destructure a subtree by staying on a node and using captures
  for interested parts. Note that the zero max start depth only limit a search
  depth for a pattern's root node but other nodes that are parts of the pattern
  may be searched at any depth what defined by the pattern structure.

  Set to @UINT32_MAX@ to remove the maximum start depth.

  > void ts_query_cursor_set_max_start_depth(TSQueryCursor *self, uint32_t max_start_depth);
-}
foreign import capi unsafe "tree_sitter/api.h ts_query_cursor_set_max_start_depth"
  ts_query_cursor_set_max_start_depth ::
    Ptr TSQueryCursor ->
    ( #{type uint32_t} ) ->
    IO ()

{- Section - Language -}

{-|
  Get another reference to the given language.

  > const TSLanguage *ts_language_copy(const TSLanguage *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_copy"
  ts_language_copy ::
    ConstPtr TSLanguage ->
    IO (ConstPtr TSLanguage)

{-|
  Free any dynamically-allocated resources for this language, if
  this is the last reference.

  > void ts_language_delete(const TSLanguage *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_delete"
  ts_language_delete ::
    ConstPtr TSLanguage ->
    IO ()

{-| C function pointer to @`ts_language_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_language_delete"
  p_ts_language_delete ::
    FunPtr (
      ConstPtr TSLanguage ->
      IO ()
    )

{-|
  Get the number of distinct node types in the language.

  > uint32_t ts_language_symbol_count(const TSLanguage *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_count"
  ts_language_symbol_count ::
    ConstPtr TSLanguage ->
    IO ( #{type uint32_t} )

{-|
  Get the number of valid states in this language.

  > uint32_t ts_language_state_count(const TSLanguage *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_state_count"
  ts_language_state_count ::
    ConstPtr TSLanguage ->
    IO ( #{type uint32_t} )

{-|
  Get a node type string for the given numerical id.

  > const char *ts_language_symbol_name(const TSLanguage *self, TSSymbol symbol);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_name"
  ts_language_symbol_name ::
    ConstPtr TSLanguage ->
    TSSymbol ->
    IO (ConstPtr CChar)

{-|
  Get the numerical id for the given node type string.

  > TSSymbol ts_language_symbol_for_name(
  >   const TSLanguage *self,
  >   const char *string,
  >   uint32_t length,
  >   bool is_named
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_for_name"
  ts_language_symbol_for_name ::
    ConstPtr TSLanguage ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    CBool ->
    IO TSSymbol

{-|
  Get the number of distinct field names in the language.

  > uint32_t ts_language_field_count(const TSLanguage *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_field_count"
  ts_language_field_count ::
    ConstPtr TSLanguage ->
    IO ( #{type uint32_t} )

{-|
  Get the field name string for the given numerical id.

  > const char *ts_language_field_name_for_id(const TSLanguage *self, TSFieldId id);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_field_name_for_id"
  ts_language_field_name_for_id ::
    ConstPtr TSLanguage ->
    TSFieldId ->
    IO (ConstPtr CChar)

{-|
  Get the numerical id for the given field name string.

  > TSFieldId ts_language_field_id_for_name(const TSLanguage *self, const char *name, uint32_t name_length);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_field_id_for_name"
  ts_language_field_id_for_name ::
    ConstPtr TSLanguage ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    IO TSFieldId

{-|
  Check whether the given node type id belongs to named nodes, anonymous nodes,
  or a hidden nodes.

  See also @`ts_node_is_named`@. Hidden nodes are never returned from the API.

  > TSSymbolType ts_language_symbol_type(const TSLanguage *self, TSSymbol symbol);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_symbol_type"
  ts_language_symbol_type ::
    ConstPtr TSLanguage ->
    TSSymbol ->
    IO TSSymbolType

{-|
  Get the ABI version number for this language. This version number is used
  to ensure that languages were generated by a compatible version of
  Tree-sitter.

  See also @`ts_parser_set_language`@.

  > uint32_t ts_language_version(const TSLanguage *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_version"
  ts_language_version ::
    ConstPtr TSLanguage ->
    IO ( #{type uint32_t} )

{-|
  Get the next parse state. Combine this with lookahead iterators to generate
  completion suggestions or valid symbols in error nodes. Use
  @`ts_node_grammar_symbol`@ for valid symbols.

  > TSStateId ts_language_next_state(const TSLanguage *self, TSStateId state, TSSymbol symbol);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_next_state"
  ts_language_next_state ::
    ConstPtr TSLanguage ->
    TSStateId ->
    TSSymbol ->
    IO TSStateId

{--------------------------------}
{- Section - Lookahead Iterator -}
{--------------------------------}

{-|
  Create a new lookahead iterator for the given language and parse state.

  This returns @NULL@ if state is invalid for the language.

  Repeatedly using @`ts_lookahead_iterator_next`@ and
  @`ts_lookahead_iterator_current_symbol`@ will generate valid symbols in the
  given parse state. Newly created lookahead iterators will contain the @ERROR@
  symbol.

  Lookahead iterators can be useful to generate suggestions and improve syntax
  error diagnostics. To get symbols valid in an ERROR node, use the lookahead
  iterator on its first leaf node state. For @MISSING@ nodes, a lookahead
  iterator created on the previous non-extra leaf node may be appropriate.

  > TSLookaheadIterator *ts_lookahead_iterator_new(const TSLanguage *self, TSStateId state);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_new"
  ts_lookahead_iterator_new ::
    ConstPtr TSLanguage ->
    TSStateId ->
    IO (Ptr TSLookaheadIterator)

{-|
  Delete a lookahead iterator freeing all the memory used.

  > void ts_lookahead_iterator_delete(TSLookaheadIterator *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_delete"
  ts_lookahead_iterator_delete ::
    Ptr TSLookaheadIterator ->
    IO ()

{-| C function pointer to @`ts_lookahead_iterator_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_lookahead_iterator_delete"
  p_ts_lookahead_iterator_delete ::
    FunPtr (
      Ptr TSLookaheadIterator ->
      IO ()
    )

{-|
  Reset the lookahead iterator to another state.

  This returns @true@ if the iterator was reset to the given state and @false@
  otherwise.

  > bool ts_lookahead_iterator_reset_state(TSLookaheadIterator *self, TSStateId state);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_reset_state"
  ts_lookahead_iterator_reset_state ::
    Ptr TSLookaheadIterator ->
    TSStateId ->
    IO CBool

{-|
  Reset the lookahead iterator.

  This returns @true@ if the language was set successfully and @false@
  otherwise.

  > bool ts_lookahead_iterator_reset(TSLookaheadIterator *self, const TSLanguage *language, TSStateId state);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_reset"
  ts_lookahead_iterator_reset ::
    Ptr TSLookaheadIterator ->
    ConstPtr TSLanguage ->
    TSStateId ->
    IO CBool

{-|
  Get the current language of the lookahead iterator.

  > const TSLanguage *ts_lookahead_iterator_language(const TSLookaheadIterator *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_language"
  ts_lookahead_iterator_language ::
    Ptr TSLookaheadIterator ->
    IO (ConstPtr TSLanguage)

{-|
  Advance the lookahead iterator to the next symbol.

  This returns @true@ if there is a new symbol and @false@ otherwise.

  > bool ts_lookahead_iterator_next(TSLookaheadIterator *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_next"
  ts_lookahead_iterator_next ::
    Ptr TSLookaheadIterator ->
    IO CBool

{-|
  Get the current symbol of the lookahead iterator;

  > TSSymbol ts_lookahead_iterator_current_symbol(const TSLookaheadIterator *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_current_symbol"
  ts_lookahead_iterator_current_symbol ::
    ConstPtr TSLookaheadIterator ->
    IO TSSymbol

{-|
  Get the current symbol type of the lookahead iterator as a null terminated
  string.

  > const char *ts_lookahead_iterator_current_symbol_name(const TSLookaheadIterator *self);
-}
foreign import capi unsafe "tree_sitter/api.h ts_lookahead_iterator_current_symbol_name"
  ts_lookahead_iterator_current_symbol_name ::
    ConstPtr TSLookaheadIterator ->
    IO (ConstPtr CChar)

{-------------------------------------}
{- Section - WebAssembly Integration -}
{-------------------------------------}

-- def TREE_SITTER_FEATURE_WASM
#ifdef TREE_SITTER_FEATURE_WASM

{-|
  > typedef struct wasm_engine_t TSWasmEngine;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSWasmEngine" #-}
  TSWasmEngine

{-|
  > typedef struct TSWasmStore TSWasmStore;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSWasmStore" #-}
  TSWasmStore

{-|
  > typedef enum {
  >   TSWasmErrorKindNone = 0,
  >   TSWasmErrorKindParse,
  >   TSWasmErrorKindCompile,
  >   TSWasmErrorKindInstantiate,
  >   TSWasmErrorKindAllocate,
  > } TSWasmErrorKind;
  -}
newtype
  {-# CTYPE "tree_sitter/api.h" "TSWasmErrorKind" #-}
  TSWasmErrorKind = TSWasmErrorKind
    { unTSWasmErrorKind :: #{type TSWasmErrorKind}
    }
    deriving (Eq, Show)
    deriving newtype (Storable)

pattern TSWasmErrorKindNone :: TSWasmErrorKind
pattern TSWasmErrorKindNone = TSWasmErrorKind ( #{const TSWasmErrorKindNone} )

pattern TSWasmErrorKindParse :: TSWasmErrorKind
pattern TSWasmErrorKindParse = TSWasmErrorKind ( #{const TSWasmErrorKindParse} )

pattern TSWasmErrorKindCompile :: TSWasmErrorKind
pattern TSWasmErrorKindCompile = TSWasmErrorKind ( #{const TSWasmErrorKindCompile} )

pattern TSWasmErrorKindInstantiate :: TSWasmErrorKind
pattern TSWasmErrorKindInstantiate = TSWasmErrorKind ( #{const TSWasmErrorKindInstantiate} )

pattern TSWasmErrorKindAllocate :: TSWasmErrorKind
pattern TSWasmErrorKindAllocate = TSWasmErrorKind ( #{const TSWasmErrorKindAllocate} )

{-# COMPLETE TSWasmErrorKindNone, TSWasmErrorKindParse, TSWasmErrorKindCompile, TSWasmErrorKindInstantiate, TSWasmErrorKindAllocate #-}

{-|
  > typedef struct {
  >   TSWasmErrorKind kind;
  >   char *message;
  > } TSWasmError;
  -}
data
  {-# CTYPE "tree_sitter/api.h" "TSWasmError" #-}
  TSWasmError = TSWasmError
    { _kind :: {-# UNPACK #-} !TSWasmErrorKind
    , _message :: {-# UNPACK #-} !ByteString
    }

peekTSWasmError :: Ptr TSWasmError -> IO TSWasmError
peekTSWasmError ptr = do
  _kind <- #{peek TSWasmError, kind} ptr
  _message <- BS.packCString ( #{ptr TSWasmError, message} ptr )
  return TSWasmError{..}

withTSWasmError :: TSWasmError -> (Ptr TSWasmError -> IO a) -> IO a
withTSWasmError TSWasmError{..} action = do
  allocaBytesAligned @TSWasmError ( #{size TSWasmError} ) ( #{alignment TSWasmError} ) $ \ptr -> do
    #{poke TSWasmError, kind} ptr _kind
    BS.useAsCString _message $ \_message_p -> do
      #{poke TSWasmError, message} ptr _message_p
      action ptr

{-|
  Create a Wasm store.

  > TSWasmStore *ts_wasm_store_new(
  >   TSWasmEngine *engine,
  >   TSWasmError *error
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_wasm_store_new"
  ts_wasm_store_new ::
    Ptr TSWasmEngine ->
    Ptr TSWasmError ->
    IO (Ptr TSWasmStore)

{-|
  Free the memory associated with the given Wasm store.

  > void ts_wasm_store_delete(TSWasmStore *);
-}
foreign import capi unsafe "tree_sitter/api.h ts_wasm_store_delete"
  ts_wasm_store_delete ::
    Ptr TSWasmStore ->
    IO ()

{-| C function pointer to @`ts_wasm_store_delete`@.
 -}
foreign import capi unsafe "tree_sitter/api.h &ts_wasm_store_delete"
  p_ts_wasm_store_delete ::
    FunPtr (
      Ptr TSWasmStore ->
      IO ()
    )

{-|
  Create a language from a buffer of Wasm. The resulting language behaves
  like any other Tree-sitter language, except that in order to use it with
  a parser, that parser must have a Wasm store. Note that the language
  can be used with any Wasm store, it doesn't need to be the same store that
  was used to originally load it.

  > const TSLanguage *ts_wasm_store_load_language(
  >   TSWasmStore *,
  >   const char *name,
  >   const char *wasm,
  >   uint32_t wasm_len,
  >   TSWasmError *error
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_wasm_store_load_language"
  ts_wasm_store_load_language ::
    Ptr TSWasmStore ->
    ConstPtr CChar ->
    ConstPtr CChar ->
    ( #{type uint32_t} ) ->
    Ptr TSWasmError ->
    IO (ConstPtr TSLanguage)

{-|
  Get the number of languages instantiated in the given wasm store.

  > size_t ts_wasm_store_language_count(const TSWasmStore *);
-}
foreign import capi unsafe "tree_sitter/api.h ts_wasm_store_language_count"
  ts_wasm_store_language_count ::
    ConstPtr TSWasmStore ->
    IO CSize

{-|
  Check if the language came from a Wasm module. If so, then in order to use
  this language with a Parser, that parser must have a Wasm store assigned.

  > bool ts_language_is_wasm(const TSLanguage *);
-}
foreign import capi unsafe "tree_sitter/api.h ts_language_is_wasm"
  ts_language_is_wasm ::
    ConstPtr TSLanguage ->
    IO CBool

{-|
  Assign the given Wasm store to the parser. A parser must have a Wasm store
  in order to use Wasm languages.

  > void ts_parser_set_wasm_store(TSParser *, TSWasmStore *);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_set_wasm_store"
  ts_parser_set_wasm_store ::
    Ptr TSParser ->
    Ptr TSWasmStore ->
    IO ()

{-|
  Remove the parser's current Wasm store and return it. This returns @NULL@ if
  the parser doesn't have a Wasm store.

  > TSWasmStore *ts_parser_take_wasm_store(TSParser *);
-}
foreign import capi unsafe "tree_sitter/api.h ts_parser_take_wasm_store"
  ts_parser_take_wasm_store ::
    Ptr TSParser ->
    IO (Ptr TSWasmStore)

-- end TREE_SITTER_FEATURE_WASM
#endif

{----------------------------------}
{- Section - Global Configuration -}
{----------------------------------}

{-|
  Set the allocation functions used by the library.

  By default, Tree-sitter uses the standard libc allocation functions,
  but aborts the process when an allocation fails. This function lets
  you supply alternative allocation functions at runtime.

  If you pass @NULL@ for any parameter, Tree-sitter will switch back to
  its default implementation of that function.

  If you call this function after the library has already been used, then
  you must ensure that either:

    1. All the existing objects have been freed.
    2. The new allocator shares its state with the old one, so it is capable of freeing memory that was allocated by the old allocator.

  > void ts_set_allocator(
  >   void *(*new_malloc)(size_t),
  >   void *(*new_calloc)(size_t, size_t),
  >   void *(*new_realloc)(void *, size_t),
  >   void (*new_free)(void *)
  > );
-}
foreign import capi unsafe "tree_sitter/api.h ts_set_allocator"
  ts_set_allocator ::
    FunPtr (CSize -> IO ()) ->
    FunPtr (CSize -> CSize -> IO ()) ->
    FunPtr (Ptr a -> CSize -> IO ()) ->
    FunPtr (Ptr a -> IO ()) ->
    IO ()
