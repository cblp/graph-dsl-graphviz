{-# LANGUAGE NamedFieldPuns #-}

module Data.Graph.Builder.GraphViz
    (GraphBuilder, Node, (-->), digraph, edge, edge', edges, edges', node) where

import Control.Monad.State
import Data.Graph.Inductive (Node)
import qualified Data.Graph.Inductive as Graph
-- import Data.Graph.Inductive.NodeMap as Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz  ( DotGraph
                      , GlobalAttributes(GraphAttrs)
                      , GraphvizParams(fmtNode, globalAttributes)
                      , graphToDot, nonClusteredParams
                      )
import Data.GraphViz.Attributes.Complete
import Data.Map
-- import Data.Text.Lazy

type Edge = Word

type Graph = Gr () Edge

data BuilderState = BuilderState
    { graph :: Graph
    , nodeAttributes :: Map Node Attributes
    , edgeAttributes :: Map Edge Attributes
    }

newBuilder :: BuilderState
newBuilder = BuilderState
    {graph = Graph.empty, nodeAttributes = mempty, edgeAttributes = mempty}

type GraphBuilderM r = State BuilderState r

type GraphBuilder = GraphBuilderM ()

node :: Attributes -> GraphBuilderM Node
node attrs = do
    builderState@BuilderState{graph, nodeAttributes} <- get
    let newNode = length nodeAttributes
    put builderState
        { graph = Graph.insNode (newNode, ()) graph
        , nodeAttributes = insert newNode attrs nodeAttributes
        }
    pure newNode

edge :: Node -> Node -> Attributes -> GraphBuilderM Edge
edge n1 n2 attrs = do
    builderState@BuilderState{edgeAttributes, graph} <- get
    let newEdge = fromIntegral $ length edgeAttributes
    put builderState
        { graph = Graph.insEdge (n1, n2, newEdge) graph
        , edgeAttributes = insert newEdge attrs edgeAttributes
        }
    pure newEdge

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
        -> DotGraph Node
digraph graphAttributes builder = let
    BuilderState{graph, nodeAttributes{-, edgeAttributes-}} =
        execState builder newBuilder
    graphvizParams = nonClusteredParams
        { fmtNode = \(nodeId, ()) -> nodeAttributes ! nodeId
        , globalAttributes = [GraphAttrs graphAttributes]
        }
    in graphToDot graphvizParams graph
