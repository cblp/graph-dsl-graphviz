{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.GraphViz.Helpers where

import Data.GraphViz.Attributes.Complete as Common  ( Attribute(Label)
                                                    , Label(HtmlLabel)
                                                    )
import Data.GraphViz.Attributes.HTML as HTML  ( Align(HLeft)
                                              , Attribute(Align)
                                              , Format(Bold)
                                              , Label(Text)
                                              , Text
                                              , TextItem(Newline, Format, Str)
                                              )
import Data.String (IsString(fromString))

instance IsString TextItem where
    fromString = Str . fromString

instance IsString Text where
    fromString = pure . fromString

-- | Bold formatter for HTML labels
formatBold :: Text -> TextItem
formatBold = Format Bold

htmlLabel :: Text -> Common.Attribute
htmlLabel = Label . HtmlLabel . Text

newline, newlineLeft :: TextItem
newline = Newline []
newlineLeft = Newline [Align HLeft]
