{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Graph.Builder.Graphviz
import Data.Graph.Builder.Graphviz.Helpers
import Data.GraphViz (DotGraph, GraphvizOutput(Svg), runGraphviz)
import Data.GraphViz.Attributes.Complete  ( Attribute(Label)
                                          , Label(HtmlLabel, StrLabel)
                                          )
import Data.GraphViz.Attributes.HTML

graph :: DotGraph Node
graph = buildGraph $ do
    task1 <- node
        [ labelHtml
              [ bold [Str "Lorem ipsum dolor sit amet,"]
              , newlineLeft
              , Str "consectetur adipisicing elit,"
              , newlineLeft
              , Str "sed do eiusmod tempor incididunt"
              , newlineLeft
              , Str "ut labore et dolore magna aliqua."
              , newlineLeft
              ]
        ]
    ipsum <- node [labelText "ipsum"]
    void $ edge_ (task1, ipsum)

main :: IO ()
main = void $ runGraphviz graph Svg "/dev/stdout"
