{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as T

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

main :: IO ()
main = do
  print "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson $ T.pack name
  print statement


