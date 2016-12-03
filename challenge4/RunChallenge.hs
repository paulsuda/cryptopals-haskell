
module Challenge4.RunChallenge (main, run) where

import qualified Shared.Hex as Hex
import Shared.Histogram (HistValue, HistArray, histMax)
import Shared.KeyScoring (singleByteXorKeyScores)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

bestXorScore :: String -> (Int, HistValue)
bestXorScore ciphertext = histMax scoreHist
  where scoreHist = singleByteXorKeyScores ciphertext

run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge4/ciphertext.txt"
  let cipherTextHexList = lines cipherTextFile
  let cipherTextList = map Hex.decode cipherTextHexList
  expectedOutputFile <- readFile "challenge4/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let decryptedString = "foo"
  putStatus("Ciphertext Entries List: (length " ++ show(length cipherTextHexList) ++ ")")
  let samplesBestXorScores = map bestXorScore cipherTextList

  putStatus("Expected Output (length " ++ show(length expectedOutput) ++ "): " ++ expectedOutput)
  if decryptedString == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
