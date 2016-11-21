module Challenge1.RunChallenge (main) where

import qualified Shared.Hex as Hex
import qualified Shared.Base64 as Base64


-- readCleanedUpFile :: String -> String
-- readCleanedUpFile (fileName) = do
--   fileContents <- readFile fileName
--   concat(lines(fileContents))

main :: IO ()
main = do
  fileInput <- readFile "challenge1/input.txt"
  let cleanedInput = concat $ lines $ fileInput
  fileExpectedOutput <- readFile "challenge1/expected_output.txt"
  let cleanedExpectedOutput = concat $ lines $ fileExpectedOutput
  let inputLength = show(length(cleanedInput))
  let decodedInput = Hex.decode(cleanedInput)
  let base64Output = Base64.encode(decodedInput)
  let decodedLength = show(length(decodedInput))
  putStrLn("Input (length " ++ inputLength ++ "): " ++ cleanedInput)
  putStrLn("Decoded (length " ++ decodedLength ++ "): " ++ decodedInput)
  putStrLn("Base64: " ++ base64Output)
  putStrLn("Expected: " ++ cleanedExpectedOutput)
  putStrLn("Got expected result? " ++ (if base64Output == cleanedExpectedOutput then "YES" else "NO"))
