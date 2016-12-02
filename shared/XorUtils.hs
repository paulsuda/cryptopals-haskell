module Shared.XorUtils (xorStrings, xorChars, xorStringChar) where

import Data.Bits (xor)

xorChars :: Char -> Char -> Char
xorChars a b = toEnum $ xor aInt bInt
  where aInt = fromEnum a
        bInt = fromEnum b

xorStrings :: String -> String -> String
xorStrings "" "" = ""
xorStrings a b = xorChars (head a) (head b) : xorStrings (tail a) (tail b)

-- Decrypt plaintext with xor key
xorStringChar :: String -> Int -> String
xorStringChar cipherText xorKey = xorStrings cipherText keyString
  where textLength = length cipherText
        keyChar = toEnum xorKey
        keyString = replicate textLength keyChar
