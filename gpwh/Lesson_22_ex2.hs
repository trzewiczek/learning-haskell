module Main where
-- import Control.Monad
import System.Environment
import System.Exit

quotes :: [String]
quotes = [
  "Quote 1",
  "Quote 2",
  "Quote 3",
  "Quote 4",
  "Quote 5"
  ]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":_) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
  where quote = quotes !! (read x - 1)

main :: IO ()
main = do
  putStrLn "Choose quotes 1-5"
  inputStream <- getContents

  let choices = lines inputStream
  mapM_ putStrLn (lookupQuote choices)

