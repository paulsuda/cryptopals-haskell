module Challenge1.RunChallenge (main) where

import Shared.Hex (decodeHex)

base64Codes :: [Char]
base64Codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

encodeBase64Char :: Int -> Char
encodeBase64Char (bits) = base64Codes !! (bits `mod` 64)

encodeBase64 :: String -> String
encodeBase64 "" = ""
encodeBase64 (a:b:c:x) = do
  let bytes = fromEnum(c) + (fromEnum(b) * (2 ^ 8)) + (fromEnum(a) * (2 ^ 16))
  [ encodeBase64Char(bytes `quot` (64 ^ 3)),  encodeBase64Char(bytes `quot` (64 ^ 2)), encodeBase64Char(bytes `quot` 64), encodeBase64Char(bytes) ] ++ encodeBase64(x)


-- readCleanedUpFile :: String -> String
-- readCleanedUpFile (fileName) = do
--   fileContents <- readFile fileName
--   concat(lines(fileContents))

main = do
  fileInput <- readFile "challenge1/input.txt"
  let cleanedInput = concat $ lines $ fileInput
  fileExpectedOutput <- readFile "challenge1/expected_output.txt"
  let cleanedExpectedOutput = concat $ lines $ fileExpectedOutput
  let inputLength = show(length(cleanedInput))
  let decodedInput = decodeHex(cleanedInput)
  let base64Output = encodeBase64(decodedInput)
  let decodedLength = show(length(decodedInput))
  putStrLn("Input (length " ++ inputLength ++ "): " ++ cleanedInput)
  putStrLn("Decoded (length " ++ decodedLength ++ "): " ++ decodedInput)
  putStrLn("Base64: " ++ base64Output)
  putStrLn("Expected: " ++ cleanedExpectedOutput)
  putStrLn("Got expected result? " ++ (if base64Output == cleanedExpectedOutput then "YES" else "NO"))
