module Challenge3.RunChallenge (main, run, testKeyScores, scoreEnglish, charHistogram) where

import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStringChar)
import Shared.TextUtils (scoreEnglish)
import Shared.Histogram (HistValue, HistArray, charHistogram, histMax)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

-- Given ciphertext and a test key returns a score how likely it is english text
scoreKey :: String -> Int -> HistValue
scoreKey cipherText xorKey = scoreEnglish possiblePlaintext
  where possiblePlaintext = xorStringChar cipherText xorKey

-- Given an array of keys to test and a cipher text argument, returns
-- a list of scores for each key.
testKeyScores :: [Int] -> String -> HistArray
testKeyScores testKeys cipherText = map (scoreKey cipherText) testKeys

-- Pass in a ciphertext and get a histogram of each of the 0 .. 255 possible
-- single byte xor keys related to it's likelyhood that it is english text.
singleByteXorKeyScores :: String -> HistArray
singleByteXorKeyScores = testKeyScores testKeys
  where testKeys = [0 .. 255]

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
