{-# LANGUAGE OverloadedStrings #-}
module Lib.Enml
    ( toNode
    ) where

import qualified CMark
import qualified Text.XML as XML

toNode :: XML.Document -> CMark.Node
toNode (XML.Document _pro root _epi) = goRoot root

goRoot :: XML.Element -> CMark.Node
goRoot (XML.Element _name _attrs children) = CMark.Node Nothing CMark.DOCUMENT (concatMap goNode children)

goNode :: XML.Node -> [CMark.Node]
goNode (XML.NodeElement e) = goElem e
goNode (XML.NodeContent "\n") = []
goNode (XML.NodeContent t) = [CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT t) []]]
goNode (XML.NodeComment _) = []
goNode (XML.NodeInstruction _) = []

goElem :: XML.Element -> [CMark.Node]
goElem (XML.Element "ul" _attrs children) = [CMark.Node Nothing (CMark.LIST lsAttr) (concatMap goNode children)]
  where lsAttr = CMark.ListAttributes CMark.BULLET_LIST True 0 CMark.PERIOD_DELIM
goElem (XML.Element "li" _attrs children) = [CMark.Node Nothing CMark.ITEM (concatMap goNode children)]
goElem (XML.Element _name _attrs children) = concatMap goNode children
