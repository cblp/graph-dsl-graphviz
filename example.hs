{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import GraphBuilder

graph :: GraphBuilder
graph = do
    task1 <- node [Label $ StrLabel "<<B>Lorem ipsum dolor sit amet,</B><BR ALIGN=LEFT/>consectetur adipisicing elit,<BR ALIGN=LEFT/>sed do eiusmod tempor incididunt<BR ALIGN=LEFT/>ut labore et dolore magna aliqua.<BR ALIGN=LEFT/>>"]
    ipsum <- node [Label $ StrLabel "ipsum"]
    void $ edge_ (task1, ipsum)

main :: IO ()
main = do
    let dotGraph = buildGraph graph
    void $ runGraphviz dotGraph DotOutput "/dev/stdout"
