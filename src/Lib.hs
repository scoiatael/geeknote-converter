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

import qualified Lib.Node as Node
import qualified Lib.Enml as Enml

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import CMark(commonmarkToNode, nodeToCommonmark)
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
toEnNoteBody = Node.toEnml . debug "toEnNoteBody" . commonmarkToNode []

toEnml :: Markdown -> Enml
toEnml = wrapXml . toEnNoteBody
  where
    wrapXml t = T.unlines [xmlVersion, enmlDoctype, T.concat [openEnNote, t, closeEnNote]]

fromEnNote :: Enml -> Markdown
fromEnNote = dump . Enml.toNode . debug "fromEnNote" . parse
  where
    dump = nodeToCommonmark [] Nothing
    parse = XML.parseText_ XML.def . L.fromStrict

fromEnml :: Enml -> Markdown
fromEnml = fromEnNote
