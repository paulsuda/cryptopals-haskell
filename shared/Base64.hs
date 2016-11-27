
module Shared.Base64 (encode, encodeChar) where

base64Codes :: String
base64Codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

encodeChar :: Int -> Char
encodeChar bits = base64Codes !! (bits `mod` 64)

encode :: String -> String
encode "" = ""
encode (a:b:c:x) = [ encodeChar(bytes `quot` (64 ^ 3)),  encodeChar(bytes `quot` (64 ^ 2)), encodeChar(bytes `quot` 64), encodeChar bytes ] ++ encode x
  where bytes = fromEnum c + (fromEnum b * (2 ^ 8)) + (fromEnum a * (2 ^ 16))
