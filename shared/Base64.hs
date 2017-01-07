
module Shared.Base64 (encode, encodeChar, decode, decodeChar) where

base64Codes :: String
base64Codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

decodeChar :: Char -> Int
decodeChar chr = if null indexesFound then 0 else head indexesFound
  where indexesFound = filter ( \x -> chr == (base64Codes !! x)) [0 .. 64]

encodeChar :: Int -> Char
encodeChar bits = base64Codes !! (bits `mod` 64)



encode :: String -> String
encode "" = ""
encode (a:b:c:x) = [ encodedCharFromInt bytes 3,  encodedCharFromInt bytes 2, encodedCharFromInt bytes 1, encodedCharFromInt bytes 0 ] ++ encode x
  where bytes = intFromBytes a b c
encode (a:b:x) = [ encodedCharFromInt bytes 3,  encodedCharFromInt bytes 2, encodedCharFromInt bytes 1, '=' ]
  where bytes = intFromBytes a b '\0'
encode (a:x) = [ encodedCharFromInt bytes 3,  encodedCharFromInt bytes 2, '=', '=' ]
  where bytes = intFromBytes a '\0' '\0'

decode :: String -> String
decode "" = ""
decode (a:b:"==") = [ byteFromInt bytes 16 ]
  where bytes = intFromEncodedChars a b '\0' '\0'
decode (a:b:c:"=") = [ byteFromInt bytes 16, byteFromInt bytes 8 ]
  where bytes = intFromEncodedChars a b c '\0'
decode (a:b:c:d:x) = [ byteFromInt bytes 16, byteFromInt bytes 8, byteFromInt bytes 0 ] ++ nextStr
  where bytes = intFromEncodedChars a b c d
        nextStr = decode x

-- - - Private - -

byteFromInt :: Int -> Int -> Char
byteFromInt bytes bitIndex = toEnum(bytes `div` (2 ^ bitIndex) `mod` 256)

intFromEncodedChars :: Char -> Char -> Char -> Char -> Int
intFromEncodedChars a b c d = decodeChar a * (64 ^ 3) + decodeChar b * (64 ^ 2) + decodeChar c * 64 + decodeChar d

intFromBytes :: Char -> Char -> Char -> Int
intFromBytes a b c = (fromEnum a * (2 ^ 16)) + (fromEnum b * (2 ^ 8)) + fromEnum c

encodedCharFromInt :: Int -> Int -> Char
encodedCharFromInt bytes charIndex = encodeChar(bytes `quot` (64 ^ charIndex))
