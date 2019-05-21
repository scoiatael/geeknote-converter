{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib.Node
    ( toEnml
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import CMark(Node(..), NodeType(..))
import Text.RawString.QQ

-- TODO: Maybe Reader would suffice?
import Control.Monad.Trans.State.Strict(evalStateT, StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Functor.Identity(Identity, runIdentity)


toEnml :: Node -> Text
toEnml = runIdentity . eval .  convert
  where
    eval = flip evalStateT Blank

data ConverterState = Blank | InBlock
type Converter = StateT ConverterState Identity

convert :: Node -> Converter Text
convert (Node _pos DOCUMENT children) = T.concat <$> mapM convert children
convert (Node _pos PARAGRAPH children) = T.concat <$> (mapM convert children >>= wrap)
  where
    wrap ch = maybeWrap ch <$> State.get
    maybeWrap ch Blank = ["<p>"] ++ ch ++ ["</p>\n"]
    maybeWrap ch InBlock = ch

convert (Node _pos (TEXT t) children) = T.concat . ([convertText t]++) <$> mapM convert children
  where
    convertText (T.stripPrefix "[x]" -> Just t') = [r|<en-todo checked="true"/>|] `T.append` t'
    convertText (T.stripPrefix "[ ]" -> Just t') = "<en-todo/>" `T.append` t'
    convertText t' = t'
convert (Node _pos (LIST _lsAttr) children) = State.withStateT (const InBlock) conversion
  where
    conversion = T.concat . (["<ul>\n"]++) .  (++ ["</ul>\n"]) <$> mapM convert children
convert (Node _pos ITEM children) = T.concat . (["<li>"]++) .  (++ ["</li>\n"]) <$> mapM convert children
convert (Node _pos STRONG children) = T.concat . (["<strong>"]++) .  (++ ["</strong>"]) <$> mapM convert children
convert (Node _pos EMPH children) = T.concat . (["<emph>"]++) .  (++ ["</emph>"]) <$> mapM convert children
convert (Node _pos (HEADING level) children) =
  T.concat . ([openTag]++) .  (++ [closeTag]) <$> mapM convert children
  where
    tag = T.concat ["h", T.pack $ show level]
    openTag = T.concat ["<", tag, ">"]
    closeTag = T.concat ["</", tag, ">\n"]
convert (Node _pos (LINK url link) children) = T.concat . (["<a href=\"", url, "\" title=\"", link, "\">"]++) .  (++ ["</a>"]) <$> mapM convert children
convert (Node _pos _type children) = T.concat <$> mapM convert children
