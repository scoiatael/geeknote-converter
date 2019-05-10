{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( fromEnml
    , fromEnNote
    , toEnml
    , toEnNoteBody
    , Enml
    , Markdown
    ) where


import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import CMark(nodeToHtml, commonmarkToNode, nodeToCommonmark)
import qualified CMark
import Text.RawString.QQ

import qualified Text.XML as XML

-- import Debug.Trace as Debug

debug :: Show a => String -> a -> a
-- debug msg a = Debug.trace (msg ++ " " ++ show a) a
debug _ = id

type Enml = Text
type Markdown = Text

xmlVersion :: Enml
xmlVersion = [r|<?xml version="1.0" encoding="UTF-8"?>|]

enmlDoctype :: Enml
enmlDoctype = [r|<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">|]

openEnNote :: Enml
openEnNote = [r|<en-note>|]

closeEnNote :: Enml
closeEnNote = [r|</en-note>|]

toEnNoteBody :: Markdown -> Enml
toEnNoteBody = dump . debug "toEnNoteBody" .commonmarkToNode []
  where
    dump = nodeToHtml []

toEnml :: Markdown -> Enml
toEnml = wrapXml . toEnNoteBody
  where
    wrapXml t = T.unlines [xmlVersion, enmlDoctype, T.concat [openEnNote, t, closeEnNote]]

convert :: XML.Document -> CMark.Node
convert (XML.Document _pro root _epi) = goRoot root

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

fromEnNote :: Enml -> Markdown
fromEnNote = dump . convert . debug "fromEnNote" . parse
  where
    dump = nodeToCommonmark [] Nothing
    parse = XML.parseText_ XML.def . L.fromStrict

fromEnml :: Enml -> Markdown
fromEnml = fromEnNote
