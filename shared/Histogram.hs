
module Shared.Histogram (HistValue, HistArray, charHistogram,
                         histMax, histMin, chiSquared, normalizeHist, unitizeHist,
                         histSet, histCombine, meanSquaredError) where

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
histCompareHelper :: HistArray -> Int -> (HistValue -> HistValue -> Bool) -> (Int, HistValue)
histCompareHelper [lastVal] lastIndex compareFn = (lastIndex , lastVal)
histCompareHelper hist index compareFn = if isNewMax then (index, thisVal) else (histMax, maxValue)
  where thisVal = head hist
        (histMax, maxValue) = histCompareHelper (tail hist) (succ index) compareFn
        isNewMax = compareFn thisVal maxValue

-- Public iterface
histMax :: HistArray -> (Int, HistValue)
histMax hist = histCompareHelper hist 0 (>)

histMin :: HistArray -> (Int, HistValue)
histMin hist = histCompareHelper hist 0 (<)

diffHist :: HistArray -> HistArray -> HistArray
diffHist [] [] = []
diffHist a b = error : diffHist (tail a) (tail b)
  where error = head a - head b


meanSquaredError :: HistArray -> HistArray -> Float
meanSquaredError a b = sum squaredErrors / fromIntegral(length squaredErrors)
  where errors = diffHist a b
        squaredErrors = map (^ 2) errors

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

-- Scale all histogram values so that the largest of all values equals 1.0
unitizeHist :: HistArray -> HistArray
unitizeHist hist = map (/ maxValue) hist
  where maxValue = maximum hist

-- Scale all histogram values so that the sum of all values equals 1.0
normalizeHist :: HistArray -> HistArray
normalizeHist hist = map (/ totalSum) hist
  where totalSum = sum hist

-- Combines two values into one by adding them. Zeroes out the "from" value.
histCombine :: HistArray -> Int -> Int -> HistArray
histCombine hist fromIndex toIndex = histSet (+ fromValue) toIndex clearedHist
  where fromValue = hist !! fromIndex
        clearedHist = histSet (const 0) fromIndex hist
