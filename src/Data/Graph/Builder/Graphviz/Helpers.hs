module Data.Graph.Builder.Graphviz.Helpers where

import Data.GraphViz.Attributes.Complete as Common  ( Attribute(Label)
                                                    , Label(HtmlLabel, StrLabel)
                                                    )
import Data.GraphViz.Attributes.HTML as HTML  ( Align(HLeft)
                                              , Attribute(Align)
                                              , Format(Bold)
                                              , Label(Text)
                                              , Text
                                              , TextItem(Format, Newline)
                                              )
import qualified Data.Text.Lazy as Text

bold :: Text -> TextItem
bold = Format Bold

labelHtml :: Text -> Common.Attribute
labelHtml = Label . HtmlLabel . Text

labelText :: Text.Text -> Common.Attribute
labelText = Label . StrLabel

newline, newlineLeft :: TextItem
newline = Newline []
newlineLeft = Newline [Align HLeft]
