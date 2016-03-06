{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Foldable
import Data.Graph.Builder.GraphViz
import Data.GraphViz.Helpers
import Data.GraphViz (DotGraph, GraphvizOutput(Svg), runGraphviz)

graph :: DotGraph Node
graph = buildGraph $ do
    t1 <- node
        [ labelHtml [ bold "Lorem ipsum dolor sit amet,",   newlineLeft
                    , "consectetur adipisicing elit,",      newlineLeft
                    , "sed do eiusmod tempor incididunt",   newlineLeft
                    , "ut labore et dolore magna aliqua.",  newlineLeft ] ]
    t2 <- node
        [ labelHtml [ bold "Ut enim ad minim veniam,",      newlineLeft
                    , "quis nostrud exercitation ullamco",  newlineLeft
                    , "laboris nisi ut aliquip",            newlineLeft
                    , "ex ea commodo consequat.",           newlineLeft ] ]
    t3 <- node
        [ labelHtml [ bold "Duis aute irure dolor",         newlineLeft
                    , "in reprehenderit in voluptate",      newlineLeft
                    , "velit esse cillum dolore",           newlineLeft
                    , "eu fugiat nulla pariatur.",          newlineLeft ] ]
    t4 <- node
        [ labelHtml [ bold "Excepteur sint occaecat",       newlineLeft
                    , "cupidatat non proident,",            newlineLeft
                    , "sunt in culpa qui officia deserunt", newlineLeft
                    , "mollit anim id est laborum.",        newlineLeft ] ]
    void $ edges' [t1, t2, t4]
    void $ edges' [t1, t3, t4]
    p <- node [label "Lorem ipsum"]
    void $ edge' (t4, p)
    for_  [ "dolor sit amet"
          , "consectetur adipisicing elit"
          , "sed do eiusmod tempor incididunt"
          , "ut labore et dolore magna aliqua"
          , "Ut enim ad minim veniam"
          , "quis nostrud exercitation ullamco"
          ] $ \text -> do
              atask <- node [label text]
              edge' (atask, p)

main :: IO ()
main = void $ runGraphviz graph Svg "/dev/stdout"
