{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Graph.Builder.GraphViz
import Data.GraphViz (DotGraph, GraphvizOutput(Svg), runGraphviz)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete (Attribute(RankDir), RankDir(FromLeft))
import Data.GraphViz.Attributes.HTML (TextItem(Str))
import Data.GraphViz.Helpers

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
    boxNode attrs = node (shape BoxShape : attrs)

    project name = boxNode [style filled, fillColor LightYellow, label name]

    task textLines = boxNode [content]
      where
        content = case textLines of
            [] -> label "<no text>"
            [oneLine] -> label oneLine
            _ -> labelHtml $ let
                firstLine : otherLines = map Str textLines
                htmlLines = formatBold [firstLine] : otherLines
                in concatMap (: [newlineLeft]) htmlLines

    taskWithProject prjNode textLines = do
        tsk <- task textLines
        void $ edge' (tsk, prjNode)
        pure tsk

main :: IO ()
main = void $ runGraphviz mytasks Svg "/dev/stdout"
