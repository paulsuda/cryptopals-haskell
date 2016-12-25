
module Challenge6.RunChallenge (run, main) where
import qualified Shared.Base64 as Base64
import Shared.XorUtils (xorStrings)
import Shared.TextUtils (repeatToLength, trimWhitespace)
import qualified Shared.BitValue as BitValue
import Shared.Views (hexShow)

keySizeRange :: (Int, Int)
keySizeRange = (2, 40)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

solveRepeatingKey :: String -> String
solveRepeatingKey cipherText = "key coming soon"

run :: (String -> IO(), String -> IO(), String -> IO()) -> IO()
run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge6/ciphertext.txt"
  let cipherTextBase64 = trimWhitespace $ concat $ lines cipherTextFile
  let cipherText = Base64.decode cipherTextBase64
  expectedOutputFile <- readFile "challenge6/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let solvedKey = solveRepeatingKey cipherText
  let xorKeyRepeated = repeatToLength (length cipherText) solvedKey
  let plainText = xorStrings cipherText xorKeyRepeated
  putStatus("Ciphertext: (length " ++ show(length cipherText) ++ ")")
  putStatus("Solved Key: " ++ solvedKey)
  putStatus("Plaintext:\n" ++ hexShow plainText)
  putStatus("Expected Output: (length " ++ show(length expectedOutput) ++ ") '" ++ expectedOutput ++ "'")
  if plainText == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
