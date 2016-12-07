module Shared.TextUtils (scoreEnglish, repeatToLength, subString, trimWhitespace) where
import Shared.Histogram (HistValue, charHistogram)

subString :: String -> Int -> Int -> String
subString _ 0 0 = ""
subString str 0 end = head str : subString (tail str) 0 (pred end)
subString str start end = subString (tail str) (pred start) (pred end)

repeatToLength :: Int -> String -> String
repeatToLength totalLength repeatString = concat(replicate replicateCount repeatString) ++ subString repeatString 0 padLength
  where replicateCount = quot totalLength repeatLength
        padLength = rem totalLength repeatLength
        repeatLength = length repeatString
--
-- wordOccurrenceCount :: String -> String -> Int
-- wordOccurrenceCount "" _ = 0
-- wordOccurrenceCount testText wordString = if isMatch then returnMatch else returnNoMatch
--   where wordLength = length wordString
--         testTextLength = length testText
--         wordCompareString = wordString
--         testTextCompareString = map (testText !!) [0 .. wordLength - 1]
--         isMatch = wordCompareString == testTextCompareString
--         -- TODO maybe there's a case insensitie cmp
--         testTextAfterFind = map (testText !!) [3 .. testTextLength - 1]
--         textTextAfterChar = map (testText !!) [1 .. testTextLength - 1]
--         returnMatch = 1 + wordOccurrenceCount testTextAfterFind wordString
--         returnNoMatch = wordOccurrenceCount textTextAfterChar wordString


-- Given some text, returns a score for likelyhood it is english text.
-- For now we just count e's and whitespace.
scoreEnglish :: String -> HistValue
scoreEnglish testText = letterE + whiteSpace + (15.0 * wordTheFreq) + (15.0 * wordAndFreq) + (2.0 * nonPrintable)
  where hist = charHistogram testText
        letterFreq = (!!) hist . fromEnum
        wordTheFreq = 1.0 -- fromIntegral(wordOccurrenceCount "the" testText) / fromIntegral textLength :: HistValue
        wordAndFreq = 1.0 -- fromIntegral(wordOccurrenceCount "and" testText) / fromIntegral textLength :: HistValue
        textLength = fromIntegral $ length testText
        nonPrintable = 1.0 - (sum(map (hist !!) [127 .. 255]) / fromIntegral textLength)
        letterE = letterFreq 'e' / fromIntegral textLength
        whiteSpace = (letterFreq '\t' + letterFreq '\n' + letterFreq ' ') / fromIntegral textLength


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
