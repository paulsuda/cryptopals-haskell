{-# LANGUAGE ParallelListComp #-}

module Shared.Histogram (HistValue, HistArray, charHistogram, histMax) where

type HistValue = Float
type HistArray = [HistValue]

-- histogram of all zeroes, n elements
zeroedHistogram :: Int -> HistArray
zeroedHistogram n = replicate n 0.0

-- Create histogram of number of occurences of each byte value in a string.
charHistogram :: String -> HistArray
charHistogram "" = zeroedHistogram 256
charHistogram testText = [ if i == histIndex then succ x else x | x <- histArray | i <- [0 .. 255] ]
  where histIndex = fromEnum $ head testText
        histArray = charHistogram $ tail testText

-- The index of the maximum value in the histogram.
histMaxHelper :: HistArray -> Int -> (Int, HistValue)
histMaxHelper [] index = (index, 0.0 :: HistValue)
histMaxHelper hist index = if isNewMax then (index, thisVal) else (histMax, maxValue)
  where thisVal = head hist
        (histMax, maxValue) = histMaxHelper (tail hist) (succ index)
        isNewMax = thisVal > maxValue

-- Public iterface
histMax :: HistArray -> (Int, HistValue)
histMax hist = histMaxHelper hist 0
