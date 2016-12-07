
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
encode (a:b:c:x) = [ encodeChar(bytes `quot` (64 ^ 3)),  encodeChar(bytes `quot` (64 ^ 2)), encodeChar(bytes `quot` 64), encodeChar bytes ] ++ encode x
  where bytes = fromEnum c + (fromEnum b * (2 ^ 8)) + (fromEnum a * (2 ^ 16))

# TODO: "=" at end of short 
decode :: String -> String
decode "" = ""
decode (a:b:c:d:x) = [ toEnum(bytes `div` (2 ^ 16) `mod` 256), toEnum(bytes `div` (2 ^ 8) `mod` 256), toEnum(bytes `mod` 256) ] ++ nextStr
  where bytes = decodeChar a * 64 * 64 * 64 + decodeChar b * 64 * 64 + decodeChar c * 64 + decodeChar d
        nextStr = decode x
