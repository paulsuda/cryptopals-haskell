module Shared.TextUtils (boundedSubString, scoreEnglish, scoreEnglishHist, englishHist, repeatToLength, subString, trimWhitespace) where

import Shared.Histogram (HistValue, HistArray, charHistogram, normalizeHist, chiSquared, histCombine)

subString :: String -> Int -> Int -> String
subString _ 0 0 = ""
subString str 0 end = head str : subString (tail str) 0 (pred end)
subString str start end = subString (tail str) (pred start) (pred end)

clampInt :: Int -> Int -> Int -> Int
clampInt val low high
  | val < low  = low
  | val > high = high
  | otherwise  = val

boundedSubString :: String -> Int -> Int -> String
boundedSubString str start end = subString str boundedStart boundedEnd
  where boundedStart = clampInt start 0 max
        boundedEnd = clampInt end boundedStart max
        max = length str


repeatToLength :: Int -> String -> String
repeatToLength totalLength repeatString = concat(replicate replicateCount repeatString) ++ subString repeatString 0 padLength
  where replicateCount = quot totalLength repeatLength
        padLength = rem totalLength repeatLength
        repeatLength = length repeatString

-- Given some text, returns a score for likelyhood it is english text.
-- For now we just count e's and whitespace.
scoreEnglish :: String -> HistValue
scoreEnglish testText = letterE + whiteSpace + (2.0 * nonPrintable)
  where hist = charHistogram testText
        letterFreq = (!!) hist . fromEnum
        textLength = fromIntegral $ length testText
        nonPrintable = 1.0 - (sum(map (hist !!) [127 .. 255]) / fromIntegral textLength)
        letterE = letterFreq 'e' / fromIntegral textLength
        whiteSpace = (letterFreq ' ' + letterFreq '\t' + letterFreq '\n') / fromIntegral textLength

scoreEnglishHist :: String -> HistValue
scoreEnglishHist testText = if invalidCount > 0 then -1.0 else chiSqScore
  where histPreprocess = normalizeHist . charHistCombineLowerToUpper . charHistCombineWhiteSpace
        observed = histPreprocess $ charHistogram testText
        expected = histPreprocess englishHist
        (chiSqScore, invalidCount) = chiSquared expected observed

-- Other tests: contains the word "the" , contains only printable chars, ratio of letters to non letters


isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n' || c == '\t'

trimWhitespaceStart :: String -> String
trimWhitespaceStart "" = ""
trimWhitespaceStart str = if isWhitespace start then nextStr else str
  where start = head str
        nextStr = trimWhitespaceStart $ tail str

trimWhitespaceEnd :: String -> String
trimWhitespaceEnd "" = ""
trimWhitespaceEnd str = if isWhitespace end then nextStr else str
  where end = last str
        nextStr = trimWhitespaceEnd $ init str

trimWhitespace :: String -> String
trimWhitespace = trimWhitespaceStart . trimWhitespaceEnd

-- Example english text char frequency histogram.
-- From Adventures of Huckleberry Finn by Mark Twain obtained by...
-- curl -o - https://www.gutenberg.org/files/76/76-0.txt | iconv -c -f UTF8 -t ASCII//TRANSLIT > example.txt
englishHist :: HistArray
englishHist = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,12363.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,108137.0,522.0,3283.0,1.0,3.0,1.0,1.0,5424.0,43.0,43.0,28.0,0.0,8199.0,3345.0,5315.0,24.0,28.0,60.0,14.0,14.0,10.0,13.0,13.0,12.0,10.0,9.0,451.0,1562.0,0.0,0.0,0.0,736.0,2.0,765.0,638.0,283.0,305.0,367.0,175.0,260.0,821.0,4340.0,548.0,42.0,257.0,348.0,349.0,276.0,397.0,10.0,281.0,966.0,1482.0,97.0,59.0,1122.0,141.0,300.0,0.0,1.0,0.0,2.0,0.0,1648.0,0.0,36199.0,6878.0,8209.0,23613.0,49263.0,7821.0,10655.0,25854.0,24314.0,693.0,5721.0,17388.0,10141.0,32785.0,36756.0,5721.0,186.0,20292.0,24563.0,41362.0,14028.0,2934.0,12305.0,387.0,10104.0,188.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

charHistCombine :: HistArray -> Char -> Char -> HistArray
charHistCombine hist toChar fromChar = histCombine hist (fromEnum toChar) (fromEnum fromChar)

charListsHistCombine :: HistArray -> [Char] -> [Char] -> HistArray
charListsHistCombine hist [] [] = hist
charListsHistCombine hist fromCharList toCharList = charHistCombine nextHist fromChar toChar
  where fromChar = head fromCharList
        toChar = head toCharList
        nextHist = charListsHistCombine hist (tail fromCharList) (tail toCharList)

charHistCombineWhiteSpace :: HistArray -> HistArray
charHistCombineWhiteSpace hist = charListsHistCombine hist whiteSpaceChars spaceChars
  where whiteSpaceChars = ['\t', '\r', '\n']
        spaceChars = replicate (length whiteSpaceChars) ' '

charHistCombineLowerToUpper :: HistArray -> HistArray
charHistCombineLowerToUpper hist = charListsHistCombine hist ['a' .. 'z'] ['A' .. 'Z']
