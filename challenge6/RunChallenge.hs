
module Challenge6.RunChallenge (run, main) where
import qualified Shared.Base64 as Base64
-- import Shared.XorUtils (xorStrings)
import Shared.TextUtils (trimWhitespace)

main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

run (putResult, putError, putStatus) = do
  cipherTextFile <- readFile "challenge6/ciphertext.txt"
  let cipherTextBase64 = trimWhitespace $ concat $ lines cipherTextFile
  let cipherText = Base64.decode cipherTextBase64
  expectedOutputFile <- readFile "challenge6/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  let plaintext = "foo"
  putStatus("Ciphertext: (length " ++ show(length cipherText) ++ ") '" ++ cipherText ++ "'")
  putStatus("Expected Output: (length " ++ show(length expectedOutput) ++ ") '" ++ expectedOutput ++ "'")
  if plaintext == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
