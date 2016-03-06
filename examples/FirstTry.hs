{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Graph.Builder.GraphViz
import Data.GraphViz (DotGraph, GraphvizOutput(Svg), runGraphviz)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Helpers

mytasks :: DotGraph Node
mytasks = digraph [RankDir FromLeft] $ do
    t1 <- node
        [ labelHtml [ formatBold "Lorem ipsum dolor sit amet,", newlineLeft
                    , "consectetur adipisicing elit,",          newlineLeft
                    , "sed do eiusmod tempor incididunt",       newlineLeft
                    , "ut labore et dolore magna aliqua.",      newlineLeft ] ]
    t2 <- node
        [ labelHtml [ formatBold "Ut enim ad minim veniam,",  newlineLeft
                    , "quis nostrud exercitation ullamco",    newlineLeft
                    , "laboris nisi ut aliquip",              newlineLeft
                    , "ex ea commodo consequat.",             newlineLeft ] ]
    t3 <- node
        [ labelHtml [ formatBold "Duis aute irure dolor", newlineLeft
                    , "in reprehenderit in voluptate",    newlineLeft
                    , "velit esse cillum dolore",         newlineLeft
                    , "eu fugiat nulla pariatur.",        newlineLeft ] ]
    t4 <- node
        [ labelHtml [ formatBold "Excepteur sint occaecat", newlineLeft
                    , "cupidatat non proident,",            newlineLeft
                    , "sunt in culpa qui officia deserunt", newlineLeft
                    , "mollit anim id est laborum.",        newlineLeft ] ]
    void $ edges' [t1, t2, t4]
    void $ edges' [t1, t3, t4]
    p <- node [label "Lorem ipsum"]
    t4 --> p
    t5 <- node [label "dolor sit amet"]
    t5 --> p
    t6 <- node [label "consectetur adipisicing elit"]
    t6 --> p
    t7 <- node [label "sed do eiusmod tempor incididunt"]
    t7 --> p
    t8 <- node [label "ut labore et dolore magna aliqua"]
    t8 --> p
    t9 <- node [label "Ut enim ad minim veniam"]
    t9 --> p
    t10 <- node [label "quis nostrud exercitation ullamco"]
    t10 --> p

main :: IO ()
main = void $ runGraphviz mytasks Svg "/dev/stdout"
