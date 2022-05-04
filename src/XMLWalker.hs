--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
--------------------------------------------------------------------------------

module XMLWalker () where

import           Text.Pandoc.Walk
import           Text.XML
import           Control.Monad ((>=>))

-- Walker for XML documents
-- Incomplete: only walks Document, Node and Element nodes

-- Walkable instance declarations
-- Walk Document
instance Walkable Document Document where
    walkM f = f
    query f = f

instance Walkable Element Document where
    walkM = walkDocumentM
    query = queryDocument

instance Walkable [Node] Document where
    walkM = walkDocumentM
    query = queryDocument

instance Walkable Node Document where
    walkM = walkDocumentM
    query = queryDocument

-- Walk Element
instance Walkable Element Element where
    walkM f e = walkElementM f e >>= f
    query f e = f e <> queryElement f e

instance Walkable [Node] Element where
    walkM = walkElementM
    query = queryElement

instance Walkable Node Element where
    walkM = walkElementM
    query = queryElement

-- Walk node list
instance {-# OVERLAPPING #-}
         Walkable [Node] [Node] where
    walkM f = traverse (walkNodeM f) >=> f
    query f nodes = f nodes <> mconcat (map (queryNode f) nodes)

-- Walk node
instance Walkable Node Node where
    walkM f n = walkNodeM f n >>= f
    query f n = f n <> queryNode f n

instance Walkable Element Node where
    walkM = walkNodeM
    query = queryNode

-- Concrete walkers

-- Helper methods to walk and query the components of a Document
walkDocumentM :: (Walkable a Element, Applicative m, Monad m) =>
    (a -> m a) -> Document -> m Document
walkDocumentM f (Document p e ms) = do
    e' <- walkM f e
    return $ Document p e' ms

queryDocument :: (Walkable a Element, Monoid c) =>
    (a -> c) -> Document -> c
queryDocument f (Document p e ms) = query f e

-- Helper methods to walk and query the components of an Elemement
walkElementM :: (Walkable a [Node], Applicative m, Monad m) =>
    (a -> m a) -> Element -> m Element
walkElementM f (Element nm attr nodes) = do
    nodes' <- walkM f nodes
    return $ Element nm attr nodes'

queryElement :: (Walkable a [Node], Monoid c) =>
    (a -> c) -> Element -> c
queryElement f (Element _ _ nodes) = query f nodes

-- Helper methods to walk and query the components of an Node
walkNodeM :: (Walkable a Element, Applicative m, Monad m) =>
    (a -> m a) -> Node -> m Node
walkNodeM f (NodeElement e) = do
    e' <- walkM f e
    return $ NodeElement e'
walkNodeM _ e = return e

queryNode :: (Walkable a Element, Monoid c) =>
    (a -> c) -> Node -> c
queryNode f (NodeElement e) = query f e
queryNode _ e = mempty
