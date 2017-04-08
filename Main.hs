module Main (main) where

import qualified Challenge1.RunChallenge
import qualified Challenge2.RunChallenge
import qualified Challenge3.RunChallenge
import qualified Challenge4.RunChallenge
import qualified Challenge5.RunChallenge
import qualified Challenge6.RunChallenge
import qualified Challenge7.RunChallenge

import Shared.Challenge (ChallengeRunner)

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment (getArgs)
import Text.PrettyPrint.ANSI.Leijen

main :: IO ()
main = do
  runChallengeArg "Challenge 1" Challenge1.RunChallenge.run
  runChallengeArg "Challenge 2" Challenge2.RunChallenge.run
  runChallengeArg "Challenge 3" Challenge3.RunChallenge.run
  runChallengeArg "Challenge 4" Challenge4.RunChallenge.run
  runChallengeArg "Challenge 5" Challenge5.RunChallenge.run
  runChallengeArg "Challenge 6" Challenge6.RunChallenge.run
  runChallengeArg "Challenge 7" Challenge7.RunChallenge.run

argMatch :: String -> IO Bool
argMatch (label) = do
   args <- getArgs
   if null args then return True else return $ (head args) == label

runChallengeArg :: String -> ChallengeRunner -> IO ()
runChallengeArg label fn = do
  argMatches <- argMatch label
  let ch = runChallenge label fn
  if argMatches then ch else return ()

runChallenge :: String -> ChallengeRunner -> IO ()
runChallenge label fn = do
  let extendedLine = replicate (60 - length label) '-'
  let labelText = "------- [ " ++ label ++ "] ---" ++ extendedLine
  putDoc $ dullyellow (text labelText) <> linebreak
  start <- getCurrentTime
  retVal <- fn greenText redText blueText
  end <- getCurrentTime
  let runSeconds = diffUTCTime end start
  putDoc $ dullyellow (text ("Run time " ++ show runSeconds ++ "sec.")) <> linebreak

blueText :: String -> IO()
blueText message = putDoc $ blue (text message) <> linebreak

redText :: String -> IO()
redText message = putDoc $ dullred (text message) <> linebreak

greenText :: String -> IO()
greenText message = putDoc $ dullgreen (text message) <> linebreak
