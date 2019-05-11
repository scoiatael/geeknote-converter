module Lib.Node
    ( toEnml
    ) where

import Data.Text(Text)
import CMark(nodeToHtml, Node)

toEnml :: Node -> Text
toEnml = nodeToHtml []
