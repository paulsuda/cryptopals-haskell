
module Shared.Views (hexShow, histShow) where

import qualified Shared.Hex as Hex
import Shared.TextUtils (boundedSubString)
import Shared.Histogram (HistValue, HistArray)

indexWidth = 5
valuesWidth = 11
valuesChartWidth =  60

-- remove unprintable chars from output for debugging data
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

-- Show a display of hex codes and ascii chars side by side.
hexShow :: String -> String
hexShow "" = ""
hexShow s = " | " ++ rawPart ++ " | " ++ "  " ++ hexPart ++ "\n" ++ hexShow remainingData
  where lineLength = 16
        rawPart = rawEscape lineData
        hexPart = Hex.encodeSeparated lineData ":"
        lineData = boundedSubString s 0 lineLength
        remainingData = boundedSubString s lineLength (length s - lineLength)

histShow :: HistArray -> String
histShow hist = summary ++ "\n" ++ values
  where summary = histSummary hist
        values = histValuesShow hist [0 .. histSize]
        histSize = length hist

padding :: Int -> Char -> String -> String
padding w padChar str = replicate (w - length str) padChar

leftPad :: Int -> String -> String
leftPad w str = padding w ' ' str ++ str

rightPad :: Int -> String -> String
rightPad w str = str ++ padding w ' ' str

histValueChartLine :: Int -> HistValue -> HistValue -> String
histValueChartLine index value max =
  leftPad indexWidth (show index) ++ " | " ++
  rightPad valuesWidth (show value) ++
  replicate (valueChartWidth value) '*' ++ "\n"
  where valueChartWidth = floor . (* valuesChartWidth) . (/ max)

histValuesShow :: HistArray -> [Int] -> String
histValuesShow hist indexes = foldl (\acc (index, value) -> acc ++
                                histValueChartLine index value (maximum hist)) "" (zip [0 .. length hist] hist)

histSummary :: HistArray -> String
histSummary hist = "Histogram Summary [ Zero Values: " ++ show zeroes ++
                   " | Mean: " ++ show mean ++
                   " | Min: " ++ show (minimum hist) ++
                   " | Max: " ++ show (maximum hist) ++ " ]"
   where zeroes = foldl (\acc x -> if x == 0.0 then succ acc else acc) 0 hist
         mean = sum hist / fromIntegral (length hist)
