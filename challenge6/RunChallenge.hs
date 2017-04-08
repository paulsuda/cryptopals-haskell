
module Challenge6.RunChallenge (run, main) where

import qualified Shared.Base64 as Base64
import qualified Shared.BitValue as BitValue
import Shared.Challenge (ChallengeRunner)
import Shared.Histogram (HistArray, HistValue, histMax, histMin, chiSquared)
import Shared.KeyScoring (singleByteXorKeyScores)
import Shared.TextUtils (repeatToLength, trimWhitespace, subString, englishHist, charHistCombineWhiteSpace, charHistCombineLowerToUpper)
import Shared.Views (hexShow, histShow, histValuesShow)
import Shared.XorUtils (xorStrings)

keySizeRange :: [Int]
keySizeRange = [2 .. 40]

main :: IO ()
main = run putStrLn putStrLn putStrLn

normalizedEditDist :: String -> Int -> HistValue
normalizedEditDist cipherText keySize = editDist / fromIntegral keySize
  where chunk i = subString cipherText (keySize * i) (keySize * succ i)
        chunkEditDist i = BitValue.hammingDist (chunk $ i * 2) (chunk $ succ $ i * 2)
        editDistList = map (fromIntegral . chunkEditDist) [0 .. 20]
        editDist = (sum editDistList) / 20.0

keySizeHistogram :: String -> HistArray
keySizeHistogram cipherText = padding ++ hist
  where hist = map (normalizedEditDist cipherText) keySizeRange
        padding = replicate firstKeySize (9999999.0)
        firstKeySize = head keySizeRange

chunkString :: String -> Int -> Int -> String
chunkString str keySize index
  | length str <= index = ""
  | otherwise = charAtIndex : chunkString remainingStr keySize index
  where charAtIndex = str !! index
        remainingStr = drop keySize str

solveForKeySize :: String -> Int -> (String, HistValue)
solveForKeySize cipherText keySize = (solvedKey, scoreAvg)
  where keyScores = map (histMin . singleByteXorKeyScores . chunkString cipherText keySize) [0 .. (keySize - 1)]
        scoreAvg = sum $ map snd keyScores
        solvedKey = map (toEnum . fst) keyScores

solveRepeatingKey :: String -> (String, HistValue)
solveRepeatingKey cipherText = solveForKeySize cipherText bestKeySize
  where (bestKeySize, bestScore) = histMin keySizeDistHist
        keySizeDistHist = keySizeHistogram cipherText

run :: ChallengeRunner
run putResult putError putStatus = do
  cipherTextFile <- readFile "challenge6/ciphertext.txt"
  let cipherTextBase64 = trimWhitespace $ concat $ lines cipherTextFile
  let cipherText = Base64.decode cipherTextBase64
  expectedOutputFile <- readFile "challenge6/expected_key.txt"
  let expectedKey = concat $ lines expectedOutputFile
  let (solvedKey, solvedKeyScore) = solveRepeatingKey cipherText
  let xorKeyRepeated = repeatToLength (length cipherText) solvedKey
  let plainText = xorStrings cipherText xorKeyRepeated :: String
  -- putStatus(histShow englishHist)
  putStatus("Plaintext:\n" ++ hexShow plainText)
  putStatus("Ciphertext: (length " ++ show(length cipherText) ++ ")")
  putStatus("Solved Key: '" ++ solvedKey ++ "' (length " ++ show(length solvedKey) ++ ", score " ++ show solvedKeyScore ++ ")")
  putStatus("Expected Key: (length " ++ show(length expectedKey) ++ ") '" ++ expectedKey ++ "'")
  if solvedKey == expectedKey then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
