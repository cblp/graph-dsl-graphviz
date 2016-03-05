{-# LANGUAGE NamedFieldPuns #-}

module Data.Graph.Builder.Graphviz
    (GraphBuilder, Node, buildGraph, edge, edge_, node) where

import Control.Monad.State
import Data.Graph.Inductive (Node)
import qualified Data.Graph.Inductive as Graph
-- import Data.Graph.Inductive.NodeMap as Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz (DotGraph, graphToDot, nonClusteredParams)
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

edge :: (Node, Node) -> Attributes -> GraphBuilderM Edge
edge (n1, n2) attrs = do
    builderState@BuilderState{edgeAttributes, graph} <- get
    let newEdge = fromIntegral $ length edgeAttributes
    put builderState
        { graph = Graph.insEdge (n1, n2, newEdge) graph
        , edgeAttributes = insert newEdge attrs edgeAttributes
        }
    pure newEdge

edge_ :: (Node, Node) -> GraphBuilderM Edge
edge_ ns = edge ns []

buildGraph :: GraphBuilder -> DotGraph Node
buildGraph builder =
    graphToDot graphvizParams . graph $ execState builder newBuilder
  where
    graphvizParams = nonClusteredParams
        -- { fmtNode = \(_node, nodeLabel) -> [Label $ StrLabel nodeLabel]
        -- }
