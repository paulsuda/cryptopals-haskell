
module Challenge6.RunChallenge (run, main) where

import qualified Shared.Base64 as Base64
import Shared.XorUtils (xorStrings)
import Shared.BitValue (hammingDist)
import Shared.TextUtils (repeatToLength, trimWhitespace, subString)
import qualified Shared.BitValue as BitValue
import Shared.Views (hexShow)
import Shared.Histogram (HistArray, HistValue, histMax)
import Shared.KeyScoring (singleByteXorKeyScores)


keySizeRange :: [Int]
keySizeRange = [2 .. 40]

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

normalizedEditDist :: String -> Int -> HistValue
normalizedEditDist cipherText keySize = fromIntegral editDist / fromIntegral keySize
  where firstChunk = subString cipherText 0 keySize
        secondChunk = subString cipherText keySize (2 * keySize)
        editDist = hammingDist firstChunk secondChunk

keySizeHistogram :: String -> HistArray
keySizeHistogram cipherText = padding ++ hist
  where hist = map (normalizedEditDist cipherText) keySizeRange
        padding = replicate firstKeySize 0.0
        firstKeySize = head keySizeRange

solveRepeatingKey :: String -> (String, HistValue)
solveRepeatingKey cipherText = solveForKeySize cipherText bestKeySize
  where (bestKeySize, bestScore) = histMax keySizeDistHist
        keySizeDistHist = keySizeHistogram cipherText

chunkString :: String -> Int -> Int -> String
chunkString str keySize index
  | length str <= index = ""
  | otherwise = charAtIndex : chunkString remainingStr keySize index
  where charAtIndex = str !! index
        remainingStr = drop keySize str



solveForKeySize :: String -> Int -> (String, HistValue)
solveForKeySize cipherText keySize = (solvedKey, scoreAvg)
  where keyScores = map (histMax . singleByteXorKeyScores . chunkString cipherText keySize) [0 .. keySize]
        scoreAvg = sum $ map snd keyScores
        solvedKey = map (toEnum . fst) keyScores



run :: (String -> IO(), String -> IO(), String -> IO()) -> IO()
run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge6/ciphertext.txt"
  let cipherTextBase64 = trimWhitespace $ concat $ lines cipherTextFile
  let cipherText = Base64.decode cipherTextBase64
  expectedOutputFile <- readFile "challenge6/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let (solvedKey, solvedKeyScore) = solveRepeatingKey cipherText
  let xorKeyRepeated = repeatToLength (length cipherText) solvedKey
  let plainText = xorStrings cipherText xorKeyRepeated
  putStatus("Plaintext:\n" ++ hexShow plainText)
  putStatus("Ciphertext: (length " ++ show(length cipherText) ++ ")")
  putStatus("Solved Key: " ++ solvedKey ++ " (length " ++ show(length solvedKey) ++ ", score " ++ show solvedKeyScore ++ ")")
  putStatus("Expected Output: (length " ++ show(length expectedOutput) ++ ") '" ++ expectedOutput ++ "'")
  if plainText == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
