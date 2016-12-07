module Main (main) where

import qualified Challenge1.RunChallenge
import qualified Challenge2.RunChallenge
import qualified Challenge3.RunChallenge
import qualified Challenge4.RunChallenge
import qualified Challenge5.RunChallenge
import Text.PrettyPrint.ANSI.Leijen

main :: IO ()
main = do
  runChallenge("Challenge 1", Challenge1.RunChallenge.run)
  runChallenge("Challenge 2", Challenge2.RunChallenge.run)
  runChallenge("Challenge 3", Challenge3.RunChallenge.run)
  -- runChallenge("Challenge 4", Challenge4.RunChallenge.run)
  runChallenge("Challenge 5", Challenge5.RunChallenge.run)

runChallenge (label, fn) = do
  let extendedLine = replicate (60 - length label) '-'
  let labelText = "------- [ " ++ label ++ "] ---" ++ extendedLine
  putDoc $ dullyellow (text labelText) <> linebreak
  fn(greenText, redText, blueText)

blueText :: String -> IO()
blueText message = putDoc $ blue (text message) <> linebreak

redText :: String -> IO()
redText message = putDoc $ dullred (text message) <> linebreak

greenText :: String -> IO()
greenText message = putDoc $ dullgreen (text message) <> linebreak
