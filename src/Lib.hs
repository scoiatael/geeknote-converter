{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( fromEnml
    , toEnml
    ) where


import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8)
import CMark(nodeToHtml, commonmarkToNode, Node(..), nodeToCommonmark, NodeType(..))
import Xeno.SAX(process)
import Control.Monad.Trans.State.Strict(execState, StateT)
import Data.Functor.Identity(Identity)
import Text.RawString.QQ

type Enml = Text
type Markdown = Text

toEnml :: Markdown -> Enml
toEnml = wrapXml . dump . commonmarkToNode []
  where
    dump = nodeToHtml []
    wrapXml t = T.unlines [xmlVersion, enmlDoctype, T.concat [openEnNote, t, closeEnNote]]

    xmlVersion = [r|<?xml version="1.0" encoding="UTF-8"?>|]
    enmlDoctype = [r|<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">|]
    openEnNote = [r|<en-note>|]
    closeEnNote = [r|</en-note>|]

type EnmlDecoder = StateT Node Identity
fromEnml :: Enml -> Markdown
fromEnml = dump . execWithInitialNode . transformXml . encodeUtf8
  where
    execWithInitialNode = flip execState $ initialNode
    initialNode = Node Nothing DOCUMENT []
    dump = nodeToCommonmark [] Nothing

    transformXml = process onOpenTag onTagAttr onEndTag onText onCloseTag onCData

    onOpenTag _ = return ()
    onTagAttr _ _ = return ()
    onEndTag _ = return ()
    onText _ = return ()
    onCloseTag _ = return ()
    onCData _ = return ()
