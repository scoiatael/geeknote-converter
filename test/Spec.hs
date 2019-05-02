{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

import Lib

emptyDoc = [r|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
<en-note></en-note>
|]

main :: IO ()
main = hspec $ do
  describe "toENML" $ do
    it "returns body wrapped in ENML" $
      toEnml "" `shouldBe` emptyDoc
