{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void, zipWithM_)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.GraphViz (GraphvizOutput(Svg), runGraphviz)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete (Attribute(RankDir), RankDir(FromLeft))
import Data.GraphViz.Attributes.HTML (TextItem(Str))
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic (Dot, DotM, (-->), digraph', graphAttrs, node')
import Data.GraphViz.Helpers (formatBold, label, labelHtml, newlineLeft)
import Data.Text.Lazy (Text)

type Node = Attributes
-- type Edge = (Node, Node)

edges' :: [Node] -> Dot Node
edges' []    = pure ()
edges' nodes = zipWithM_ (-->) nodes (tail nodes)

mytasks :: DotGraph Node
mytasks = digraph' $ do
    graphAttrs [RankDir FromLeft]

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
    boxNode :: Attributes -> DotM Node Node
    boxNode attrs = node' (shape BoxShape : style filled : attrs) $> attrs

    project :: Text -> DotM Node Node
    project name = boxNode [fillColor Pink, label name]

    task :: [Text] -> DotM Node Node
    task textLines = boxNode [fillColor LightYellow, content]
      where
        content = case textLines of
            []        -> label "<no text>"
            [oneLine] -> label oneLine
            _         -> labelHtml $ let
                firstLine : otherLines = map Str textLines
                htmlLines = formatBold [firstLine] : otherLines
                in concatMap (: [newlineLeft]) htmlLines

    taskWithProject prjNode textLines = do
        tsk <- task textLines
        tsk --> prjNode
        pure tsk

main :: IO ()
main = void $ runGraphviz mytasks Svg "/dev/stdout"
