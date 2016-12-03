module Shared.TextUtils (scoreEnglish) where
import Shared.Histogram (HistValue, charHistogram)


-- Given some text, returns a score for likelyhood it is english text.
-- For now we just count e's and whitespace.
scoreEnglish :: String -> HistValue
scoreEnglish testText = letterE + whiteSpace
  where hist = charHistogram testText
        letterFreq = (!!) hist . fromEnum
        textLength = fromIntegral $ length testText
        letterE = letterFreq 'e' / textLength
        whiteSpace = (letterFreq '\t' + letterFreq '\n' + letterFreq ' ') / textLength

-- Other tests: contains the word "the" , contains only printable chars, ratio of letters to non letters
