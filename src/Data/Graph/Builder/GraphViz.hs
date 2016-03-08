{-# LANGUAGE NamedFieldPuns #-}

module Data.Graph.Builder.GraphViz
    ( Edge, Graph, GraphBuilder, Node
    , (-->), digraph, edge, edge', edges, edges', node
    ) where

import Control.Monad.State
import qualified Data.Graph.Inductive as Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz  ( DotGraph
                      , GlobalAttributes(GraphAttrs)
                      , GraphvizParams(fmtNode, globalAttributes)
                      , graphToDot, nonClusteredParams
                      )
import Data.GraphViz.Attributes.Complete
import Data.Map

type NodeImpl = Inductive.Node
newtype Node = Node NodeImpl

type EdgeImpl = Word
newtype Edge = Edge EdgeImpl

type GraphImpl = Gr () EdgeImpl
type Graph = DotGraph Inductive.Node

data BuilderState = BuilderState
    { graph :: GraphImpl
    , nodeAttributes :: Map NodeImpl Attributes
    , edgeAttributes :: Map EdgeImpl Attributes
    }

newBuilder :: BuilderState
newBuilder = BuilderState
    {graph = Inductive.empty, nodeAttributes = mempty, edgeAttributes = mempty}

type GraphBuilderM r = State BuilderState r

type GraphBuilder = GraphBuilderM ()

node :: Attributes -> GraphBuilderM Node
node attrs = do
    builderState@BuilderState{graph, nodeAttributes} <- get
    let newNode = length nodeAttributes
    put builderState
        { graph = Inductive.insNode (newNode, ()) graph
        , nodeAttributes = insert newNode attrs nodeAttributes
        }
    pure $ Node newNode

edge :: Node -> Node -> Attributes -> GraphBuilderM Edge
edge (Node n1) (Node n2) attrs = do
    builderState@BuilderState{edgeAttributes, graph} <- get
    let newEdge = fromIntegral $ length edgeAttributes
    put builderState
        { graph = Inductive.insEdge (n1, n2, newEdge) graph
        , edgeAttributes = insert newEdge attrs edgeAttributes
        }
    pure $ Edge newEdge

-- | 'edge' without attributes
edge' :: Node -> Node -> GraphBuilderM Edge
edge' n1 n2 = edge n1 n2 []

-- | 'edge' without attributes and result
(-->) :: Node -> Node -> GraphBuilder
n1 --> n2 = void $ edge n1 n2 []

edges :: [Node] -> Attributes -> GraphBuilderM [Edge]
edges []    _     = pure []
edges nodes attrs = zipWithM (\n1 n2 -> edge n1 n2 attrs) nodes (tail nodes)

edges' :: [Node] -> GraphBuilderM [Edge]
edges' nodes = edges nodes []

digraph :: Attributes -- ^ graph attributes
        -> GraphBuilder
        -> Graph
digraph graphAttributes builder = let
    BuilderState{graph, nodeAttributes{-, edgeAttributes-}} =
        execState builder newBuilder
    graphvizParams = nonClusteredParams
        { fmtNode = \(nodeId, ()) -> nodeAttributes ! nodeId
        , globalAttributes = [GraphAttrs graphAttributes]
        }
    in graphToDot graphvizParams graph
