module Challenge3.RunChallenge (main, run) where

import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStrings)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

-- Given some text, returns a score for likelyhood it is english text.
englishScore :: String -> Rational
englishScore testText = 0.5

-- Decrypt plaintext with xor key
decryptXorSimple :: String -> Int -> String
decryptXorSimple cipherText xorKey = xorStrings cipherText keyString
  where textLength = length cipherText
        keyChar = toEnum xorKey
        keyString = replicate textLength keyChar

-- Given ciphertext and a test key returns a score
scoreKey :: String -> Int -> Rational
scoreKey cipherText xorKey = englishScore possiblePlaintext
  where possiblePlaintext = decryptXorSimple cipherText xorKey

-- Given an array of keys to test and a cipher text argument, returns
-- a list of scores for each key.
testKeyScores :: [Int] -> String -> [Rational]
testKeyScores testKeys cipherText = map (scoreKey cipherText) testKeys

run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge3/ciphertext.txt"
  let cipherTextHex =  concat $ lines cipherTextFile
  let cipherText = Hex.decode cipherTextHex
  expectedOutputFile <- readFile "challenge3/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let testKeys = [0 .. 255]
  let scores = testKeyScores testKeys cipherText
  let textKeyScores = zip testKeys scores
  let xorKey = 33 -- TODO!
  let decryptedString = decryptXorSimple cipherText xorKey
  putStatus("Ciphertext (length " ++ show(length cipherTextHex) ++ "): " ++ cipherTextHex)
  putStatus("Ciphertext String: " ++ cipherText)
  putStatus("Expected Output (length " ++ show(length expectedOutput) ++ "): " ++ expectedOutput)
  if decryptedString == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
