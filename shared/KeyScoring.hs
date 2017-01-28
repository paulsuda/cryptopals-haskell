module Shared.KeyScoring (singleByteXorKeyScores) where

import Shared.Histogram (HistValue, HistArray)
import Shared.TextUtils (scoreEnglishFrequency)
import Shared.XorUtils (xorStringChar)

-- Given ciphertext and a test key returns a score how likely it is english text
scoreKey :: String -> Int -> HistValue
scoreKey cipherText xorKey = scoreEnglishFrequency possiblePlaintext
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
