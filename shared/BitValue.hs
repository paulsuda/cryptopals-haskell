
module Shared.BitValue (list, index) where
import Shared.XorUtils (xorStrings)

index :: Int -> Int -> Int
index i val
  | i >= 0 && i < 8 = (val `quot` (2 ^ i)) `mod` 2
  | otherwise = 0
    -- fail "Index must be between 0 and 7 inclusive for index."

list :: Int -> [Int]
list val = [ index i val | i <- [0..7] ]

countOnes :: Char -> Int
countOnes c = sum bits
  where val = fromEnum c
        bits = list val

countStringOneBits :: String -> Int
countStringOneBits "" = 0
countStringOneBits (c:s) = countOnes c + countStringOneBits s

hammingDist :: String -> String -> Int
hammingDist a b = countStringOneBits differenceBits
  where differenceBits = xorStrings a b
