{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Foldable
import Data.Graph.Builder.GraphViz
import Data.GraphViz (DotGraph, GraphvizOutput(Svg), runGraphviz)
import Data.GraphViz.Attributes.Colors.SVG
import Data.GraphViz.Attributes.Complete as Common
import Data.GraphViz.Attributes.HTML as HTML
import Data.GraphViz.Helpers
import Data.Text.Lazy ()

mytasks :: DotGraph Node
mytasks = digraph [RankDir FromLeft] $ do
    p <- project "Lorem ipsum"
    t1 <- task
        [ "Lorem ipsum dolor sit amet,"
        , "consectetur adipisicing elit,", "sed do eiusmod tempor incididunt"
        , "ut labore et dolore magna aliqua." ]
    t2 <- task
        [ "Ut enim ad minim veniam,"
        , "quis nostrud exercitation ullamco", "laboris nisi ut aliquip"
        , "ex ea commodo consequat." ]
    t3 <- task
        [ "Duis aute irure dolor"
        , "in reprehenderit in voluptate", "velit esse cillum dolore"
        , "eu fugiat nulla pariatur." ]
    t4 <- taskWithProject p
        [ "Excepteur sint occaecat"
        , "cupidatat non proident,", "sunt in culpa qui officia deserunt"
        , "mollit anim id est laborum." ]
    void $ edges' [t1, t2, t4]
    void $ edges' [t1, t3, t4]
    traverse_ (taskWithProject p . pure)
        [ "dolor sit amet"
        , "consectetur adipisicing elit"
        , "sed do eiusmod tempor incididunt"
        , "ut labore et dolore magna aliqua"
        , "Ut enim ad minim veniam"
        , "quis nostrud exercitation ullamco" ]
  where
    box attrs = node (Shape BoxShape : attrs)
    project name = box [Style [SItem Filled []], FillColor lightYellow, label name]
    lightYellow = [toWC $ SVGColor LightYellow]
    task textLines = box [content]
      where
        content = case textLines of
            [] -> label "<no text>"
            [oneLine] -> label oneLine
            _ -> labelHtml $ let
                firstLine : otherLines = map Str textLines
                textLines' = bold [firstLine] : otherLines
                in concatMap (: [newlineLeft]) textLines'
    taskWithProject prjNode textLines = do
        tsk <- task textLines
        void $ edge' (tsk, prjNode)
        pure tsk

main :: IO ()
main = void $ runGraphviz mytasks Svg "/dev/stdout"
