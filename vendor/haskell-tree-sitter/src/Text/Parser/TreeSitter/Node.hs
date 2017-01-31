{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Text.Parser.TreeSitter.Node where

import Foreign
import Foreign.C
import Foreign.CStorable
import GHC.Generics
import Text.Parser.TreeSitter.Document

data Node = Node (Ptr ()) CSize CSize CSize
  deriving (Show, Eq, Generic, CStorable)

instance Storable Node where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke

foreign import ccall "src/bridge.c ts_document_root_node_p" ts_document_root_node_p :: Ptr Document -> Ptr Node -> IO ()
foreign import ccall "src/bridge.c ts_node_p_name" ts_node_p_name :: Ptr Node -> Ptr Document -> IO CString
foreign import ccall "src/bridge.c ts_node_p_child_count" ts_node_p_child_count :: Ptr Node -> IO CSize
foreign import ccall "src/bridge.c ts_node_p_named_child_count" ts_node_p_named_child_count :: Ptr Node -> IO CSize
foreign import ccall "src/bridge.c ts_node_p_child" ts_node_p_child :: Ptr Node -> CSize -> Ptr Node -> IO CSize
foreign import ccall "src/bridge.c ts_node_p_named_child" ts_node_p_named_child :: Ptr Node -> CSize -> Ptr Node -> IO CSize
foreign import ccall "src/bridge.c ts_node_p_start_char" ts_node_p_start_char :: Ptr Node -> CSize
foreign import ccall "src/bridge.c ts_node_p_end_char" ts_node_p_end_char :: Ptr Node -> CSize

foreign import ccall "src/bridge.c ts_node_p_start_point_row" ts_node_p_start_point_row :: Ptr Node -> CSize
foreign import ccall "src/bridge.c ts_node_p_start_point_column" ts_node_p_start_point_column :: Ptr Node -> CSize
foreign import ccall "src/bridge.c ts_node_p_end_point_row" ts_node_p_end_point_row :: Ptr Node -> CSize
foreign import ccall "src/bridge.c ts_node_p_end_point_column" ts_node_p_end_point_column :: Ptr Node -> CSize
