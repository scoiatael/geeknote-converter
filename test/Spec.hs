{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Text.RawString.QQ
import qualified Data.Text as T

import Lib

prelude :: Enml
prelude = [r|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
|]

emptyDoc :: Enml
emptyDoc = T.concat [prelude, [r|<en-note></en-note>|], "\n"]

main :: IO ()
main = hspec $ do
  describe "toENML" $ do
    it "returns body wrapped in ENML" $
      toEnml "\n" `shouldBe` emptyDoc

  describe "fromEnml" $ do
    it "parses empty note" $
      fromEnml emptyDoc `shouldBe` "\n"

  describe "toEnNoteBody" $ do
    it "converts Md unordered list" $
      toEnNoteBody "* 1\n* 2\n*3" `shouldBe` "<ul>\n<li>1</li>\n<li>2\n*3</li>\n</ul>\n"

  describe "fromEnNote" $ do
    it "converts Html unordered list" $ do
      fromEnNote "<en-note><ul>\n<li>1</li>\n<li>2\n*3</li>\n</ul>\n</en-note>" `shouldBe` "* 1\n* 2\n*3"
