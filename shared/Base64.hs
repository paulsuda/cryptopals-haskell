
module Shared.Base64 (encode, encodeChar) where

base64Codes :: [Char]
base64Codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

encodeChar :: Int -> Char
encodeChar (bits) = base64Codes !! (bits `mod` 64)

encode :: String -> String
encode "" = ""
encode (a:b:c:x) = do
  let bytes = fromEnum(c) + (fromEnum(b) * (2 ^ 8)) + (fromEnum(a) * (2 ^ 16))
  [ encodeChar(bytes `quot` (64 ^ 3)),  encodeChar(bytes `quot` (64 ^ 2)), encodeChar(bytes `quot` 64), encodeChar(bytes) ] ++ encode(x)
