module Challenge3.RunChallenge (main, run, testKeyScores, scoreEnglish, charHistogram) where

import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStringChar)
import Shared.TextUtils (scoreEnglish)
import Shared.Histogram (HistValue, HistArray, charHistogram)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

maxIndexHelper :: HistArray -> Int -> (Int, HistValue)
maxIndexHelper [] index = (index, 0.0 :: HistValue)
maxIndexHelper hist index = if isNewMax then (index, thisVal) else (maxIndex, maxValue)
  where thisVal = head hist
        (maxIndex, maxValue) = maxIndexHelper (tail hist) (succ index)
        isNewMax = thisVal > maxValue

maxIndex :: HistArray -> (Int, HistValue)
maxIndex hist = maxIndexHelper hist 0

-- Given ciphertext and a test key returns a score
scoreKey :: String -> Int -> HistValue
scoreKey cipherText xorKey = scoreEnglish possiblePlaintext
  where possiblePlaintext = xorStringChar cipherText xorKey

-- Given an array of keys to test and a cipher text argument, returns
-- a list of scores for each key.
testKeyScores :: [Int] -> String -> HistArray
testKeyScores testKeys cipherText = map (scoreKey cipherText) testKeys

run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge3/ciphertext.txt"
  let cipherTextHex =  concat $ lines cipherTextFile
  let cipherText = Hex.decode cipherTextHex
  expectedOutputFile <- readFile "challenge3/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let testKeys = [0 .. 255]
  let scores = testKeyScores testKeys cipherText
  -- let textKeyScores = zip testKeys scores
  let (xorKey, bestScore) = maxIndex scores
  let decryptedString = xorStringChar cipherText xorKey
  putStatus("Ciphertext (length " ++ show(length cipherTextHex) ++ "): " ++ cipherTextHex)
  putStatus("Ciphertext String: " ++ cipherText)
  putStatus("XOR Key: " ++ show xorKey ++ " Score: " ++ show bestScore)
  putStatus("Decrypted Output: " ++ decryptedString)
  putStatus("Expected Output (length " ++ show(length expectedOutput) ++ "): " ++ expectedOutput)
  if decryptedString == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
