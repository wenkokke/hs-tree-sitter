{-# LANGUAGE RoleAnnotations #-}

module TreeSitter.Internal.ST (
  -- * Types
  FieldId (..),
  FieldName (..),
  GrammarType (..),
  InputEdit (..),
  Point (..),
  StateId (..),
  Symbol (..),
  SymbolType (..),
  Language,
  Node,
  TreeCursor,

  -- * Tree
  treeRootNode,
  treeRootNodeWithOffset,

  -- * Node
  nodeType,
  nodeTypeAsString,
  nodeSymbol,
  nodeLanguage,
  nodeGrammarType,
  nodeGrammarTypeAsString,
  nodeGrammarSymbol,
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

  -- * Language
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
) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Word (Word32)
import TreeSitter.Internal (
  FieldId (..),
  FieldName (..),
  GrammarType (..),
  InputEdit (..),
  Point (..),
  StateId (..),
  Symbol (..),
  SymbolType (..),
 )
import TreeSitter.Internal qualified as TSIO

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}

--------------------------------------------------------------------------------

-- * Types

newtype Language s = WrapIOLanguage TSIO.Language

type role Language nominal

newtype Node s = WrapIONode TSIO.Node

type role Node nominal

newtype TreeCursor s = WrapIOTreeCursor TSIO.TreeCursor

type role TreeCursor nominal

-- * Tree

-- | See @`TSIO.treeRootNode`@.
treeRootNode :: TSIO.Tree -> ST s (Node s)
treeRootNode = unsafeIOToST . coerce . TSIO.treeRootNode
{-# INLINE treeRootNode #-}

-- | See @`TSIO.treeRootNodeWithOffset`@.
treeRootNodeWithOffset :: TSIO.Tree -> Word32 -> TSIO.Point -> ST s (Node s)
treeRootNodeWithOffset = ((unsafeIOToST .) .) . coerce TSIO.treeRootNodeWithOffset
{-# INLINE treeRootNodeWithOffset #-}

-- * Node

-- | See @`TSIO.nodeType`@.
nodeType :: Node s -> ST s ByteString
nodeType = unsafeIOToST . coerce TSIO.nodeType
{-# INLINE nodeType #-}

-- -- | See @`TSIO.nodeType`@.
nodeTypeAsString :: Node s -> ST s String
nodeTypeAsString = unsafeIOToST . coerce TSIO.nodeTypeAsString
{-# INLINE nodeTypeAsString #-}

-- | See @`TSIO.nodeSymbol`@.
nodeSymbol :: Node s -> ST s TSIO.Symbol
nodeSymbol = unsafeIOToST . coerce TSIO.nodeSymbol
{-# INLINE nodeSymbol #-}

-- | See @`TSIO.nodeLanguage`@.
nodeLanguage :: Node s -> ST s TSIO.Language
nodeLanguage = unsafeIOToST . coerce TSIO.nodeLanguage
{-# INLINE nodeLanguage #-}

-- | See @`TSIO.nodeGrammarType`@.
nodeGrammarType :: Node s -> ST s GrammarType
nodeGrammarType = unsafeIOToST . coerce TSIO.nodeGrammarType
{-# INLINE nodeGrammarType #-}

-- | See @`TSIO.nodeGrammarType`@.
nodeGrammarTypeAsString :: Node s -> ST s String
nodeGrammarTypeAsString = unsafeIOToST . coerce TSIO.nodeGrammarTypeAsString
{-# INLINE nodeGrammarTypeAsString #-}

-- | See @`TSIO.nodeGrammarSymbol`@.
nodeGrammarSymbol :: Node s -> ST s Symbol
nodeGrammarSymbol = unsafeIOToST . coerce TSIO.nodeGrammarSymbol
{-# INLINE nodeGrammarSymbol #-}

-- | See @`TSIO.nodeStartByte`@.
nodeStartByte :: Node s -> ST s Word32
nodeStartByte = unsafeIOToST . coerce TSIO.nodeStartByte
{-# INLINE nodeStartByte #-}

-- | See @`TSIO.nodeStartPoint`@.
nodeStartPoint :: Node s -> ST s Point
nodeStartPoint = unsafeIOToST . coerce TSIO.nodeStartPoint
{-# INLINE nodeStartPoint #-}

-- | See @`TSIO.nodeEndByte`@.
nodeEndByte :: Node s -> ST s Word32
nodeEndByte = unsafeIOToST . coerce TSIO.nodeEndByte
{-# INLINE nodeEndByte #-}

-- | See @`TSIO.nodeEndPoint`@.
nodeEndPoint :: Node s -> ST s Point
nodeEndPoint = unsafeIOToST . coerce TSIO.nodeEndPoint
{-# INLINE nodeEndPoint #-}

-- | See @`TSIO.nodeString`@.
showNode :: Node s -> ST s ByteString
showNode = unsafeIOToST . coerce TSIO.showNode
{-# INLINE showNode #-}

-- | See @`TSIO.nodeString`@.
showNodeAsString :: Node s -> ST s String
showNodeAsString = unsafeIOToST . coerce TSIO.showNodeAsString
{-# INLINE showNodeAsString #-}

-- | See @`TSIO.nodeIsNull`@.
nodeIsNull :: Node s -> ST s Bool
nodeIsNull = unsafeIOToST . coerce TSIO.nodeIsNull
{-# INLINE nodeIsNull #-}

-- | See @`TSIO.nodeIsNamed`@.
nodeIsNamed :: Node s -> ST s Bool
nodeIsNamed = unsafeIOToST . coerce TSIO.nodeIsNamed
{-# INLINE nodeIsNamed #-}

-- | See @`TSIO.nodeIsMissing`@.
nodeIsMissing :: Node s -> ST s Bool
nodeIsMissing = unsafeIOToST . coerce TSIO.nodeIsMissing
{-# INLINE nodeIsMissing #-}

-- | See @`TSIO.nodeIsExtra`@.
nodeIsExtra :: Node s -> ST s Bool
nodeIsExtra = unsafeIOToST . coerce TSIO.nodeIsExtra
{-# INLINE nodeIsExtra #-}

-- | See @`TSIO.nodeHasChanges`@.
nodeHasChanges :: Node s -> ST s Bool
nodeHasChanges = unsafeIOToST . coerce TSIO.nodeHasChanges
{-# INLINE nodeHasChanges #-}

-- | See @`TSIO.nodeHasError`@.
nodeHasError :: Node s -> ST s Bool
nodeHasError = unsafeIOToST . coerce TSIO.nodeHasError
{-# INLINE nodeHasError #-}

-- | See @`TSIO.nodeIsError`@.
nodeIsError :: Node s -> ST s Bool
nodeIsError = unsafeIOToST . coerce TSIO.nodeIsError
{-# INLINE nodeIsError #-}

-- | See @`TSIO.nodeParseState`@.
nodeParseState :: Node s -> ST s StateId
nodeParseState = unsafeIOToST . coerce TSIO.nodeParseState
{-# INLINE nodeParseState #-}

-- | See @`TSIO.nodeNextParseState`@.
nodeNextParseState :: Node s -> ST s StateId
nodeNextParseState = unsafeIOToST . coerce TSIO.nodeNextParseState
{-# INLINE nodeNextParseState #-}

-- | See @`TSIO.nodeParent`@.
nodeParent :: Node s -> ST s (Node s)
nodeParent = unsafeIOToST . coerce TSIO.nodeParent
{-# INLINE nodeParent #-}

-- | See @`TSIO.nodeChildWithDescen[dant`@.
nodeChildWithDescendant :: Node s -> Node s -> ST s (Node s)
nodeChildWithDescendant = (unsafeIOToST .) . coerce TSIO.nodeChildWithDescendant
{-# INLINE nodeChildWithDescendant #-}

-- | See @`TSIO.nodeChild`@.
nodeChild :: Node s -> Word32 -> ST s (Node s)
nodeChild = (unsafeIOToST .) . coerce TSIO.nodeChild
{-# INLINE nodeChild #-}

-- | See @`TSIO.nodeFieldNameForChild`@.
nodeFieldNameForChild :: Node s -> Word32 -> ST s FieldName
nodeFieldNameForChild = (unsafeIOToST .) . coerce TSIO.nodeFieldNameForChild
{-# INLINE nodeFieldNameForChild #-}

-- | See @`TSIO.nodeFieldNameForChild`@.
nodeFieldNameForChildAsString :: Node s -> Word32 -> ST s String
nodeFieldNameForChildAsString = (unsafeIOToST .) . coerce TSIO.nodeFieldNameForChildAsString
{-# INLINE nodeFieldNameForChildAsString #-}

-- | See @`TSIO.nodeFieldNameForNamedChild`@.
nodeFieldNameForNamedChild :: Node s -> Word32 -> ST s FieldName
nodeFieldNameForNamedChild = (unsafeIOToST .) . coerce TSIO.nodeFieldNameForNamedChild
{-# INLINE nodeFieldNameForNamedChild #-}

-- | See @`TSIO.nodeFieldNameForNamedChild`@.
nodeFieldNameForNamedChildAsString :: Node s -> Word32 -> ST s String
nodeFieldNameForNamedChildAsString = (unsafeIOToST .) . coerce TSIO.nodeFieldNameForNamedChildAsString
{-# INLINE nodeFieldNameForNamedChildAsString #-}

-- | See @`TSIO.nodeChildCount`@.
nodeChildCount :: Node s -> ST s Word32
nodeChildCount = unsafeIOToST . coerce TSIO.nodeChildCount
{-# INLINE nodeChildCount #-}

-- | See @`TSIO.nodeNamedChild`@.
nodeNamedChild :: Node s -> Word32 -> ST s (Node s)
nodeNamedChild = (unsafeIOToST .) . coerce TSIO.nodeNamedChild
{-# INLINE nodeNamedChild #-}

-- | See @`TSIO.nodeNamedChildCount`@.
nodeNamedChildCount :: Node s -> ST s Word32
nodeNamedChildCount = unsafeIOToST . coerce TSIO.nodeNamedChildCount
{-# INLINE nodeNamedChildCount #-}

-- | See @`TSIO.nodeChildByFieldName`@.
nodeChildByFieldName :: Node s -> FieldName -> ST s (Node s)
nodeChildByFieldName = (unsafeIOToST .) . coerce TSIO.nodeChildByFieldName
{-# INLINE nodeChildByFieldName #-}

-- | See @`TSIO.nodeChildByFieldId`@.
nodeChildByFieldId :: Node s -> FieldId -> ST s (Node s)
nodeChildByFieldId = (unsafeIOToST .) . coerce TSIO.nodeChildByFieldId
{-# INLINE nodeChildByFieldId #-}

-- | See @`TSIO.nodeNextSibling`@.
nodeNextSibling :: Node s -> ST s (Node s)
nodeNextSibling = unsafeIOToST . coerce TSIO.nodeNextSibling
{-# INLINE nodeNextSibling #-}

-- | See @`TSIO.nodePrevSibling`@.
nodePrevSibling :: Node s -> ST s (Node s)
nodePrevSibling = unsafeIOToST . coerce TSIO.nodePrevSibling
{-# INLINE nodePrevSibling #-}

-- | See @`TSIO.nodeNextNamedSibling`@.
nodeNextNamedSibling :: Node s -> ST s (Node s)
nodeNextNamedSibling = unsafeIOToST . coerce TSIO.nodeNextNamedSibling
{-# INLINE nodeNextNamedSibling #-}

-- | See @`TSIO.nodePrevNamedSibling`@.
nodePrevNamedSibling :: Node s -> ST s (Node s)
nodePrevNamedSibling = unsafeIOToST . coerce TSIO.nodePrevNamedSibling
{-# INLINE nodePrevNamedSibling #-}

-- | See @`TSIO.nodeFirstChildForByte`@.
nodeFirstChildForByte :: Node s -> Word32 -> ST s (Node s)
nodeFirstChildForByte = (unsafeIOToST .) . coerce TSIO.nodeFirstChildForByte
{-# INLINE nodeFirstChildForByte #-}

-- | See @`TSIO.nodeFirstNamedChildForByte`@.
nodeFirstNamedChildForByte :: Node s -> Word32 -> ST s (Node s)
nodeFirstNamedChildForByte = (unsafeIOToST .) . coerce TSIO.nodeFirstNamedChildForByte
{-# INLINE nodeFirstNamedChildForByte #-}

-- | See @`TSIO.nodeDescendantCount`@.
nodeDescendantCount :: Node s -> ST s Word32
nodeDescendantCount = unsafeIOToST . coerce TSIO.nodeDescendantCount
{-# INLINE nodeDescendantCount #-}

-- | See @`TSIO.nodeDescendantForByteRange`@.
nodeDescendantForByteRange :: Node s -> Word32 -> Word32 -> ST s (Node s)
nodeDescendantForByteRange = ((unsafeIOToST .) .) . coerce TSIO.nodeDescendantForByteRange
{-# INLINE nodeDescendantForByteRange #-}

-- | See @`TSIO.nodeDescendantForPointRange`@.
nodeDescendantForPointRange :: Node s -> Point -> Point -> ST s (Node s)
nodeDescendantForPointRange = ((unsafeIOToST .) .) . coerce TSIO.nodeDescendantForPointRange
{-# INLINE nodeDescendantForPointRange #-}

-- | See @`TSIO.nodeNamedDescendantForByteRange`@.
nodeNamedDescendantForByteRange :: Node s -> Word32 -> Word32 -> ST s (Node s)
nodeNamedDescendantForByteRange = ((unsafeIOToST .) .) . coerce TSIO.nodeNamedDescendantForByteRange
{-# INLINE nodeNamedDescendantForByteRange #-}

-- | See @`TSIO.nodeNamedDescendantForPointRange`@.
nodeNamedDescendantForPointRange :: Node s -> Point -> Point -> ST s (Node s)
nodeNamedDescendantForPointRange = ((unsafeIOToST .) .) . coerce TSIO.nodeNamedDescendantForPointRange
{-# INLINE nodeNamedDescendantForPointRange #-}

-- | See @`TSIO.nodeEdit`@.
nodeEdit :: Node s -> InputEdit -> ST s ()
nodeEdit = (unsafeIOToST .) . coerce TSIO.nodeEdit
{-# INLINE nodeEdit #-}

-- | See @`TSIO.nodeEq`@.
nodeEq :: Node s -> Node s -> ST s Bool
nodeEq = (unsafeIOToST .) . coerce TSIO.nodeEq
{-# INLINE nodeEq #-}

-- * TreeCursor

-- | See @`TSIO.treeCursorNew`@.
treeCursorNew :: Node s -> ST s (TreeCursor s)
treeCursorNew = unsafeIOToST . coerce TSIO.treeCursorNew

-- | See @`TSIO.treeCursorReset`@.
treeCursorReset :: TreeCursor s -> Node s -> ST s ()
treeCursorReset = (unsafeIOToST .) . coerce TSIO.treeCursorReset

-- | See @`TSIO.treeCursorResetTo`@.
treeCursorResetTo :: TreeCursor s -> TreeCursor s -> ST s ()
treeCursorResetTo = (unsafeIOToST .) . coerce TSIO.treeCursorResetTo

-- | See @`TSIO.treeCursorCurrentNode`@.
treeCursorCurrentNode :: TreeCursor s -> ST s (Node s)
treeCursorCurrentNode = unsafeIOToST . coerce TSIO.treeCursorCurrentNode

-- | See @`TSIO.treeCursorCurrentFieldName`@.
treeCursorCurrentFieldName :: TreeCursor s -> ST s (Maybe FieldName)
treeCursorCurrentFieldName = unsafeIOToST . coerce TSIO.treeCursorCurrentFieldName

-- | See @`TSIO.treeCursorCurrentFieldId`@.
treeCursorCurrentFieldId :: TreeCursor s -> ST s (Maybe FieldId)
treeCursorCurrentFieldId = unsafeIOToST . coerce TSIO.treeCursorCurrentFieldId

-- | See @`TSIO.treeCursorGotoParent`@.
treeCursorGotoParent :: TreeCursor s -> ST s Bool
treeCursorGotoParent = unsafeIOToST . coerce TSIO.treeCursorGotoParent

-- | See @`TSIO.treeCursorGotoNextSibling`@.
treeCursorGotoNextSibling :: TreeCursor s -> ST s Bool
treeCursorGotoNextSibling = unsafeIOToST . coerce TSIO.treeCursorGotoNextSibling

-- | See @`TSIO.treeCursorGotoPreviousSibling`@.
treeCursorGotoPreviousSibling :: TreeCursor s -> ST s Bool
treeCursorGotoPreviousSibling = unsafeIOToST . coerce TSIO.treeCursorGotoPreviousSibling

-- | See @`TSIO.treeCursorGotoFirstChild`@.
treeCursorGotoFirstChild :: TreeCursor s -> ST s Bool
treeCursorGotoFirstChild = unsafeIOToST . coerce TSIO.treeCursorGotoFirstChild

-- | See @`TSIO.treeCursorGotoLastChild`@.
treeCursorGotoLastChild :: TreeCursor s -> ST s Bool
treeCursorGotoLastChild = unsafeIOToST . coerce TSIO.treeCursorGotoLastChild

-- | See @`TSIO.treeCursorGotoDescendant`@.
treeCursorGotoDescendant :: TreeCursor s -> Word32 -> ST s ()
treeCursorGotoDescendant = (unsafeIOToST .) . coerce TSIO.treeCursorGotoDescendant

-- | See @`TSIO.treeCursorCurrentDescendantIndex`@.
treeCursorCurrentDescendantIndex :: TreeCursor s -> ST s Word32
treeCursorCurrentDescendantIndex = unsafeIOToST . coerce TSIO.treeCursorCurrentDescendantIndex

-- | See @`TSIO.treeCursorCurrentDepth`@.
treeCursorCurrentDepth :: TreeCursor s -> ST s Word32
treeCursorCurrentDepth = unsafeIOToST . coerce TSIO.treeCursorCurrentDepth

-- | See @`TSIO.treeCursorGotoFirstChildForByte`@.
treeCursorGotoFirstChildForByte :: TreeCursor s -> Word32 -> ST s Int64
treeCursorGotoFirstChildForByte = (unsafeIOToST .) . coerce TSIO.treeCursorGotoFirstChildForByte

-- | See @`TSIO.treeCursorGotoFirstChildForPoint`@.
treeCursorGotoFirstChildForPoint :: TreeCursor s -> Point -> ST s Int64
treeCursorGotoFirstChildForPoint = (unsafeIOToST .) . coerce TSIO.treeCursorGotoFirstChildForPoint

-- | See @`TSIO.treeCursorCopy`@.
treeCursorCopy :: TreeCursor s -> ST s (TreeCursor s)
treeCursorCopy = unsafeIOToST . coerce TSIO.treeCursorCopy

-- * Language

-- | See @`C.ts_language_copy`@.
languageCopy :: Language s -> ST s (Language s)
languageCopy = unsafeIOToST . coerce TSIO.languageCopy

-- | See @`C.ts_language_symbol_count`@.
languageSymbolCount :: Language s -> ST s Word32
languageSymbolCount = unsafeIOToST . coerce TSIO.languageSymbolCount

-- | See @`C.ts_language_state_count`@.
languageStateCount :: Language s -> ST s Word32
languageStateCount = unsafeIOToST . coerce TSIO.languageStateCount

-- | See @`C.ts_language_symbol_name`@.
languageSymbolName :: Language s -> Symbol -> ST s ByteString
languageSymbolName = (unsafeIOToST .) . coerce TSIO.languageSymbolName

-- | See @`C.ts_language_symbol_for_name`@.
languageSymbolForGrammarType :: Language s -> ByteString -> Bool -> ST s Symbol
languageSymbolForGrammarType = ((unsafeIOToST .) .) . coerce TSIO.languageSymbolForGrammarType

-- | See @`C.ts_language_field_count`@.
languageFieldCount :: Language s -> ST s Word32
languageFieldCount = unsafeIOToST . coerce TSIO.languageFieldCount

-- | See @`C.ts_language_field_name_for_id`@.
languageFieldNameForId :: Language s -> FieldId -> ST s ByteString
languageFieldNameForId = (unsafeIOToST .) . coerce TSIO.languageFieldNameForId

-- | See @`C.ts_language_field_id_for_name`@.
languageFieldIdForName :: Language s -> ByteString -> ST s FieldId
languageFieldIdForName = (unsafeIOToST .) . coerce TSIO.languageFieldIdForName

-- | See @`C.ts_language_symbol_type`@.
languageSymbolType :: Language s -> Symbol -> ST s SymbolType
languageSymbolType = (unsafeIOToST .) . coerce TSIO.languageSymbolType

-- | See @`C.ts_language_version`@.
languageVersion :: Language s -> ST s Word32
languageVersion = unsafeIOToST . coerce TSIO.languageVersion

-- | See @`C.ts_language_next_state`@.
languageNextState :: Language s -> StateId -> Symbol -> ST s StateId
languageNextState = ((unsafeIOToST .) .) . coerce TSIO.languageNextState
