
module Challenge4.RunChallenge (main, run) where

import qualified Shared.Hex as Hex
import Shared.Histogram (HistValue, HistArray, histMax)
import Shared.KeyScoring (singleByteXorKeyScores)
import Shared.XorUtils (xorStringChar)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

bestXorScore :: String -> (Int, HistValue)
bestXorScore ciphertext = histMax $ singleByteXorKeyScores ciphertext

run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge4/ciphertext.txt"
  let cipherTextHexList = lines cipherTextFile
  let cipherTextList = map Hex.decode cipherTextHexList
  expectedOutputFile <- readFile "challenge4/expected_output.txt"
  let expectedOutput = expectedOutputFile
  putStatus("Ciphertext Entries List: (length " ++ show(length cipherTextHexList) ++ ")")
  let samplesBestXorScores = map bestXorScore cipherTextList
  let sampleBestScores = map snd samplesBestXorScores
  let sampleBestKeys = map fst samplesBestXorScores
  let (bestSampleIndex, bestSampleScore) = histMax sampleBestScores
  let bestXorKey = sampleBestKeys !! bestSampleIndex
  let bestCipherText = cipherTextList !! bestSampleIndex
  let decryptedString = xorStringChar bestCipherText bestXorKey
  putStatus("Best sample index: " ++ show bestSampleIndex ++ " with score: " ++ show bestSampleScore)
  putStatus("Best XOR Key: " ++ show bestXorKey ++ " decrypts to: '" ++ decryptedString ++ "'")
  putStatus("Expected Output (length " ++ show(length expectedOutput) ++ "): '" ++ expectedOutput ++ "'")
  if decryptedString == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
