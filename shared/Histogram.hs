{-# LANGUAGE ParallelListComp #-}

module Shared.Histogram (HistValue, HistArray, charHistogram,
                         histMax, chiSquared, normalizeHist,
                         histSet, histCombine) where

type HistValue = Float
type HistArray = [HistValue]

-- histogram of all zeroes, n elements
zeroedHistogram :: Int -> HistArray
zeroedHistogram n = replicate n 0.0

-- Apply a function to the nth value in the histogram
histSet :: (HistValue -> HistValue) -> Int -> HistArray -> HistArray
histSet fn index hist = firstPart ++ [fn value] ++ lastPart
  where firstPart = take index hist
        remainingPart = drop index hist
        lastPart = tail remainingPart
        value = head remainingPart

-- Create histogram of number of occurences of each byte value in a string.
charHistogram :: String -> HistArray
charHistogram "" = zeroedHistogram 256
charHistogram testText = histSet succ histIndex histArray
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

-- Compare using chi sq
-- sum( (obs[i] - expected[i])^2 / expected[i] ) = x^2
chiSquared :: HistArray -> HistArray -> (Float, Int)
chiSquared [] [] = (0.0, 0)
chiSquared (0.0 : expectedHist) (0.0 : observedHist) = chiSquared expectedHist observedHist
chiSquared (0.0 : expectedHist) (bad : observedHist) = (fst remaining, succ $ snd remaining)
  where remaining = chiSquared expectedHist observedHist
chiSquared (expected : expectedHist) (observed : observedHist) = (thisPart + fst remaining, snd remaining)
  where thisPart = (observed - expected) ^ 2 / expected
        remaining = chiSquared expectedHist observedHist

normalizeHist :: HistArray -> HistArray
normalizeHist hist = map (/ totalSum) hist
  where totalSum = sum hist

-- Combines two values into one by adding them. Zeroes out the "from" value.
histCombine :: HistArray -> Int -> Int -> HistArray
histCombine hist toIndex fromIndex = histSet (+ fromValue) toIndex clearedHist
  where fromValue = hist !! fromIndex
        clearedHist = histSet (const 0) fromIndex hist
