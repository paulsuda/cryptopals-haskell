module Challenge3.RunChallenge (main, run) where

import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStringChar)
import Shared.Histogram (HistValue, HistArray, histMax)
import Shared.KeyScoring (singleByteXorKeyScores)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge3/ciphertext.txt"
  let cipherTextHex =  concat $ lines cipherTextFile
  let cipherText = Hex.decode cipherTextHex
  expectedOutputFile <- readFile "challenge3/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let scores = singleByteXorKeyScores cipherText
  let (xorKey, bestScore) = histMax scores
  let decryptedString = xorStringChar cipherText xorKey
  putStatus("Ciphertext (length " ++ show(length cipherTextHex) ++ "): " ++ cipherTextHex)
  putStatus("Ciphertext String: " ++ cipherText)
  putStatus("XOR Key: " ++ show xorKey ++ " Score: " ++ show bestScore)
  putStatus("Decrypted Output: " ++ decryptedString)
  putStatus("Expected Output (length " ++ show(length expectedOutput) ++ "): " ++ expectedOutput)
  if decryptedString == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
