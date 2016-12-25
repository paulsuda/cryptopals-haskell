
module Shared.Views (hexShow) where

import qualified Shared.Hex as Hex
import Shared.TextUtils (boundedSubString)

rawEscapeChar :: Char -> Char
rawEscapeChar c
  | n >= 127  = '.'
  | n < 32    = '_'
  | otherwise = c
  where n = fromEnum c

-- Show a string, subbing out nonprintable chars with something else.
rawEscape :: String -> String
rawEscape "" = ""
rawEscape s = thisChar : rawEscape (tail s)
  where thisChar = rawEscapeChar (head s)

hexShow :: String -> String
hexShow "" = ""
hexShow s = " | " ++ rawPart ++ " | " ++ "  " ++ hexPart ++ "\n" ++ hexShow remainingData
  where lineLength = 16
        rawPart = rawEscape lineData
        hexPart = Hex.encodeSeparated lineData ":"
        lineData = boundedSubString s 0 lineLength
        remainingData = boundedSubString s lineLength (length s - lineLength)
