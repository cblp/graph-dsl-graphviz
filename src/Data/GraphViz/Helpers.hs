{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.GraphViz.Helpers where

import Data.GraphViz.Attributes.Complete as Common  ( Attribute(Label)
                                                    , Label(HtmlLabel, StrLabel)
                                                    )
import Data.GraphViz.Attributes.HTML as HTML  ( Align(HLeft)
                                              , Attribute(Align)
                                              , Format(Bold)
                                              , Label(Text)
                                              , Text
                                              , TextItem(Format, Newline, Str)
                                              )
import qualified Data.Text.Lazy as Text
import GHC.Exts (IsString(fromString))

instance IsString TextItem where
    fromString = Str . fromString

instance IsString Text where
    fromString = pure . fromString

bold :: Text -> TextItem
bold = Format Bold

labelHtml :: Text -> Common.Attribute
labelHtml = Label . HtmlLabel . Text

label :: Text.Text -> Common.Attribute
label = Label . StrLabel

newline, newlineLeft :: TextItem
newline = Newline []
newlineLeft = Newline [Align HLeft]
