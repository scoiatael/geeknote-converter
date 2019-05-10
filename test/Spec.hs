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

xmlList :: Enml
xmlList = "<ul>\n<li>1</li>\n<li>2</li>\n<li>3</li>\n</ul>\n"

mdList :: Markdown
mdList = "  - 1\n  - 2\n  - 3\n"

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
      toEnNoteBody mdList `shouldBe` xmlList

  describe "fromEnNote" $ do
    it "converts Html unordered list" $ do
      fromEnNote (T.concat ["<en-note>", xmlList, "</en-note>"]) `shouldBe` mdList
