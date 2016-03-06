{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Graph.Builder.GraphViz
import Data.GraphViz.Helpers
import Data.GraphViz (DotGraph, GraphvizOutput(Svg), runGraphviz)

graph :: DotGraph Node
graph = buildGraph $ do
    task1 <- node
        [ labelHtml
              [ bold "Lorem ipsum dolor sit amet,"
              , newlineLeft
              , "consectetur adipisicing elit,"
              , newlineLeft
              , "sed do eiusmod tempor incididunt"
              , newlineLeft
              , "ut labore et dolore magna aliqua."
              , newlineLeft
              ]
        ]
    ipsum <- node [labelText "ipsum"]
    void $ edge_ (task1, ipsum)

main :: IO ()
main = void $ runGraphviz graph Svg "/dev/stdout"
