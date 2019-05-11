{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Lib.Node
    ( toEnml
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import CMark(Node(..), NodeType(..))

-- TODO: Maybe Reader would suffice?
import Control.Monad.Trans.State.Strict(evalStateT, StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Functor.Identity(Identity, runIdentity)


toEnml :: Node -> Text
toEnml = runIdentity . eval .  convert
  where
    eval = flip evalStateT Blank

data ConverterState = Blank | InList
type Converter = StateT ConverterState Identity

convert :: Node -> Converter Text
convert (Node _pos DOCUMENT children) = T.concat <$> mapM convert children
convert (Node _pos PARAGRAPH children) = T.concat <$> (mapM convert children >>= wrap)
  where
    wrap ch = maybeWrap ch <$> State.get
    maybeWrap ch Blank = ["<p>"] ++ ch ++ ["</p>\n"]
    maybeWrap ch InList = ch

convert (Node _pos (TEXT t) children) = T.concat . ([convertText t]++) <$> mapM convert children
  where
    convertText (T.stripPrefix "[ ]" -> Just t') = "<en-todo/>" `T.append` t'
    convertText t' = t'
convert (Node _pos (LIST _lsAttr) children) = State.withStateT (const InList) conversion
  where
    conversion = T.concat . (["<ul>\n"]++) .  (++ ["</ul>\n"]) <$> mapM convert children
convert (Node _pos ITEM children) = T.concat . (["<li>"]++) .  (++ ["</li>\n"]) <$> mapM convert children
