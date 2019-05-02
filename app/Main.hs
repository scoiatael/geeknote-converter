module Main where

import Lib
import Console.Options (defaultMain, programName, programDescription, command, action)
import Data.Text(unpack, pack)

main :: IO ()
main = defaultMain $ do
  programName "geeknote-converter"
  programDescription "Convert between Evernote ENML and Markdown"
  command "fromE" $ do
    action $ \_ -> do
      input <- getContents
      process fromEnml input

  command "toE" $ do
    action $ \_ -> do
      input <- getContents
      process toEnml input

  where process f = putStrLn . unpack . f . pack
