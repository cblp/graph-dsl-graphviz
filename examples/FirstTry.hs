{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Graph.Builder.GraphViz
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Helpers

mytasks :: Graph
mytasks = digraph [RankDir FromLeft] $ do
    t1 <- node
        [ htmlLabel [ formatBold "Lorem ipsum dolor sit amet,", newlineLeft
                    , "consectetur adipisicing elit,",          newlineLeft
                    , "sed do eiusmod tempor incididunt",       newlineLeft
                    , "ut labore et dolore magna aliqua.",      newlineLeft ] ]
    t2 <- node
        [ htmlLabel [ formatBold "Ut enim ad minim veniam,",  newlineLeft
                    , "quis nostrud exercitation ullamco",    newlineLeft
                    , "laboris nisi ut aliquip",              newlineLeft
                    , "ex ea commodo consequat.",             newlineLeft ] ]
    t3 <- node
        [ htmlLabel [ formatBold "Duis aute irure dolor", newlineLeft
                    , "in reprehenderit in voluptate",    newlineLeft
                    , "velit esse cillum dolore",         newlineLeft
                    , "eu fugiat nulla pariatur.",        newlineLeft ] ]
    t4 <- node
        [ htmlLabel [ formatBold "Excepteur sint occaecat", newlineLeft
                    , "cupidatat non proident,",            newlineLeft
                    , "sunt in culpa qui officia deserunt", newlineLeft
                    , "mollit anim id est laborum.",        newlineLeft ] ]
    void $ edges' [t1, t2, t4]
    void $ edges' [t1, t3, t4]
    p <- node [textLabel "Lorem ipsum"]
    t4 --> p
    t5 <- node [textLabel "dolor sit amet"]
    t5 --> p
    t6 <- node [textLabel "consectetur adipisicing elit"]
    t6 --> p
    t7 <- node [textLabel "sed do eiusmod tempor incididunt"]
    t7 --> p
    t8 <- node [textLabel "ut labore et dolore magna aliqua"]
    t8 --> p
    t9 <- node [textLabel "Ut enim ad minim veniam"]
    t9 --> p
    t10 <- node [textLabel "quis nostrud exercitation ullamco"]
    t10 --> p

main :: IO ()
main = void $ runGraphviz mytasks Svg "/dev/stdout"
