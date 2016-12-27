module Shared.TextUtils (boundedSubString, scoreEnglish, repeatToLength, subString, trimWhitespace) where
import Shared.Histogram (HistValue, charHistogram)

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
        whiteSpace = letterFreq ' ' / fromIntegral textLength


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
