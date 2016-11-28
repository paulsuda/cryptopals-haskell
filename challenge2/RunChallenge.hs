module Challenge2.RunChallenge (main, run) where

import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStrings)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

run (putResult, putError, putStatus) = do
  startingDataFile <- readFile "challenge2/starting_data.txt"
  expectedOutputFile <- readFile "challenge2/expected_output.txt"
  xorDataFile <- readFile "challenge2/xor_data.txt"
  let startingData = concat $ lines startingDataFile
  let expectedOutput = concat $ lines expectedOutputFile
  let xorData = concat $ lines xorDataFile
  let startingString = Hex.decode startingData
  let expectedOutputString = Hex.decode expectedOutput
  let xorDataString = Hex.decode xorData
  let xorOutputString = xorStrings startingString xorDataString
  let xorOutputHexString = Hex.encode xorOutputString
  putStatus("Starting Data (length " ++ show(length startingData) ++ "): " ++ startingData)
  putStatus("Starting Data String: " ++ startingString)
  putStatus("XOR Data (length " ++ show(length xorData) ++ "): " ++ xorData)
  putStatus("XOR Data String: " ++ xorDataString)
  putStatus("Expected Output (length " ++ show(length expectedOutput) ++ "): " ++ expectedOutput)
  putStatus("Expected Output String: " ++ expectedOutputString)
  putStatus("Calculated Output (length " ++ show(length xorOutputHexString) ++ "): " ++ xorOutputHexString)
  putStatus("Calculated Output String (length " ++ show(length xorOutputString) ++ "): " ++ xorOutputString)
  if xorOutputString == expectedOutputString then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
