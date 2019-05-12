{-# LANGUAGE OverloadedStrings #-}
module Lib.Enml
    ( toNode
    ) where

import qualified Data.Map as M
import Data.Text(Text)
import qualified CMark
import qualified Text.XML as XML

import Control.Monad.Trans.State.Strict(evalStateT, StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Functor.Identity(Identity, runIdentity)

toNode :: XML.Document -> CMark.Node
toNode (XML.Document _pro root _epi) = runIdentity . eval $  goRoot root
  where
    eval = flip evalStateT ""

type Converter = StateT Text Identity
goRoot :: XML.Element -> Converter CMark.Node
goRoot e = CMark.Node Nothing CMark.DOCUMENT <$> goElem e

goNode :: XML.Node -> Converter [CMark.Node]
goNode (XML.NodeElement e) = goElem e
goNode (XML.NodeContent "\n") = return []
goNode (XML.NodeContent t) = buildWithPrefix <$> State.get >>= resetState
  where
    buildWithPrefix pre =
        [ CMark.Node Nothing (CMark.CUSTOM_INLINE pre "") []
        , CMark.Node Nothing (CMark.TEXT t) []
        ]
    resetState v = State.put "" >> return v
goNode (XML.NodeComment _) = return []
goNode (XML.NodeInstruction _) = return []

goElem :: XML.Element -> Converter [CMark.Node]
goElem (XML.Element "ul" _attrs children) =
  return . CMark.Node Nothing (CMark.LIST lsAttr) . concat <$> mapM goNode children
  where lsAttr = CMark.ListAttributes CMark.BULLET_LIST True 0 CMark.PERIOD_DELIM
goElem (XML.Element "li" _attrs children) =
  return . CMark.Node Nothing CMark.ITEM . return . CMark.Node Nothing CMark.PARAGRAPH . concat <$> mapM goNode children
goElem (XML.Element "en-todo" attrs _children) | "checked" `M.lookup` attrs == Just "true" = State.put "[x]" >> return []
goElem (XML.Element "en-todo" _attrs _children) = State.put "[ ]" >> return []
goElem (XML.Element "h1" _attrs children) = return . CMark.Node Nothing (CMark.HEADING 1) . concat <$> mapM goNode children
goElem (XML.Element "h2" _attrs children) = return . CMark.Node Nothing (CMark.HEADING 2) . concat <$> mapM goNode children
goElem (XML.Element "h3" _attrs children) = return . CMark.Node Nothing (CMark.HEADING 3) . concat <$> mapM goNode children
goElem (XML.Element "p" _attrs children) = return . CMark.Node Nothing CMark.PARAGRAPH . concat <$> mapM goNode children
goElem (XML.Element _name _attrs children) = concat <$> mapM goNode children
