{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Text.RawString.QQ
import qualified Data.Text as T
import System.Directory(listDirectory)

import Lib

prelude :: Enml
prelude = [r|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
|]

emptyDoc :: Enml
emptyDoc = T.concat [prelude, [r|<en-note>|], "\n", [r|</en-note>|], "\n"]

xmlList :: Enml
xmlList = "<ul>\n<li>1</li>\n<li>2</li>\n<li>3</li>\n</ul>\n"

mdList :: Markdown
mdList = "  - 1\n  - 2\n  - 3\n"

mdChecklist :: Markdown
mdChecklist = "  - [ ] item1\n  - [ ] item2\n"

xmlChecklist :: Enml
xmlChecklist = "<ul>\n<li><en-todo/> item1</li>\n<li><en-todo/> item2</li>\n</ul>\n"

note :: Enml -> Enml
note inner = (T.concat ["<en-note>", inner, "</en-note>"])

main :: IO ()
main = hspec $ do
  describe "toENML" $
    it "returns body wrapped in ENML" $
      toEnml "\n" `shouldBe` emptyDoc

  describe "fromEnml" $
    it "parses empty note" $
      fromEnml emptyDoc `shouldBe` "\n"

  describe "toEnNoteBody" $ do
    it "converts unordered list" $
      toEnNoteBody mdList `shouldBe` xmlList

    it "converts paragraph" $
      toEnNoteBody "A paragraph\n" `shouldBe`  "<p>A paragraph</p>\n"

    it "converts checklist" $
      toEnNoteBody mdChecklist `shouldBe` xmlChecklist

    it "converts checked checklist" $
      toEnNoteBody "  - [x] item1\n" `shouldBe` T.concat ["<ul>\n<li>", [r|<en-todo checked="true"/> item1|], "</li>\n</ul>\n"]

    it "converts header" $
      toEnNoteBody "# A header\n" `shouldBe`  "<h1>A header</h1>\n"

    it "converts sub-header" $
      toEnNoteBody "## A header\n" `shouldBe`  "<h2>A header</h2>\n"

    it "converts sub-sub-header" $
      toEnNoteBody "### A header\n" `shouldBe`  "<h3>A header</h3>\n"

    it "converts bold" $
      toEnNoteBody "**bold**\n" `shouldBe`  "<p><strong>bold</strong></p>\n"

    it "converts italic" $
      toEnNoteBody "*emphasized*\n" `shouldBe`  "<p><emph>emphasized</emph></p>\n"

    it "converts links" $
      toEnNoteBody "[example](http://example.test)\n"  `shouldBe` "<p><a href=\"http://example.test\" title=\"\">example</a></p>\n"

    it "converts images" $
      toEnNoteBody "![a cat](http://placekitten.com/200/300)" `shouldBe` "<p><img src=\"http://placekitten.com/200/300\" alt=\"a cat\"></img></p>\n"

    it "converts quotes" $
      toEnNoteBody "> ![a cat](http://placekitten.com/200/300)" `shouldBe` "<blockquote><p><img src=\"http://placekitten.com/200/300\" alt=\"a cat\"></img></p>\n</blockquote>"

  describe "fromEnNote" $ do
    it "converts unordered list" $
      fromEnNote (note xmlList) `shouldBe` mdList

    it "converts paragraph" $
      fromEnNote "<p>A paragraph</p>\n" `shouldBe` "A paragraph\n"

    it "converts checklist" $
      fromEnNote (note xmlChecklist) `shouldBe` mdChecklist

    it "converts checked checklist" $
      fromEnNote (note [r|<ul><li><en-todo checked="true"/> item1</li></ul>|]) `shouldBe` "  - [x] item1\n"

    it "works with paragraph after checklist" $
      fromEnNote (note [r|<ul><li><en-todo/> item1</li></ul><p>paragraph</p>|]) `shouldBe` "  - [ ] item1\n\nparagraph\n"

    it "converts header" $
      fromEnNote (note "<h1>A header</h1>\n")`shouldBe` "# A header\n"

    it "converts sub-header" $
      fromEnNote (note "<h2>A header</h2>\n")`shouldBe` "## A header\n"

    it "converts sub-sub-header" $
      fromEnNote (note "<h3>A header</h3>\n")`shouldBe` "### A header\n"

    it "converts bold" $
      fromEnNote (note  "<p><strong>bold</strong></p>\n") `shouldBe` "**bold**\n"

    it "converts italic" $
      fromEnNote (note  "<p><emph>emphasized</emph></p>\n") `shouldBe` "*emphasized*\n"

    it "converts links" $
      fromEnNote (note  "<p><a href='http://example.test'>example</a></p>\n") `shouldBe` "[example](http://example.test)\n"

    it "converts images" $
      fromEnNote (note  "<p><img src=\"http://placekitten.com/200/300\" alt=\"a cat\"></img></p>\n") `shouldBe` "![a cat](http://placekitten.com/200/300)\n"

    it "converts quotes" $
      fromEnNote (note "<blockquote><p><img src=\"http://placekitten.com/200/300\" alt=\"a cat\"></img></p>\n</blockquote>") `shouldBe`"> ![a cat](http://placekitten.com/200/300)\n"

  describe "examples" $ do
    let examplesDir = "test/examples"
    examples <- runIO $ listDirectory examplesDir

    let testExample exampleFolder = do
          let readToText fname = T.pack <$> runIO ( readFile $ examplesDir ++ "/" ++ exampleFolder ++ "/" ++ fname)
          md <- readToText "test.md"
          xml <- readToText "test.xml"

          it ("works for " ++ exampleFolder) $ do
            toEnml md `shouldBe` xml
            fromEnml xml `shouldBe` md

    mapM_ testExample examples
