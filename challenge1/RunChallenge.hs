module Challenge1.RunChallenge (main, run) where

import qualified Shared.Hex as Hex
import qualified Shared.Base64 as Base64

main :: IO ()
main = do
  run(putStrLn, putStrLn, putStrLn)

run (putResult, putError, putStatus) = do
  fileInput <- readFile "challenge1/input.txt"
  let cleanedInput = concat $ lines $ fileInput
  fileExpectedOutput <- readFile "challenge1/expected_output.txt"
  let cleanedExpectedOutput = concat $ lines $ fileExpectedOutput
  let inputLength = show(length(cleanedInput))
  let decodedInput = Hex.decode(cleanedInput)
  let base64Output = Base64.encode(decodedInput)
  let decodedLength = show(length(decodedInput))
  putStatus("Input (length " ++ inputLength ++ "): " ++ cleanedInput)
  putStatus("Decoded (length " ++ decodedLength ++ "): " ++ decodedInput)
  putStatus("Base64: " ++ base64Output)
  putStatus("Expected: " ++ cleanedExpectedOutput)
  if base64Output == cleanedExpectedOutput then putResult("OK! Expected result.")
  else putError("ERROR! Result not as expected.")
