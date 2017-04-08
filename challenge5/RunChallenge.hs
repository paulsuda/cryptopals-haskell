
module Challenge5.RunChallenge (run, main) where
import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStrings)
import Shared.TextUtils (repeatToLength, trimWhitespace)

main :: IO ()
main = run putStrLn putStrLn putStrLn

run putResult putError putStatus = do
  plainTextFile <- readFile "challenge5/plaintext.txt"
  let plainText = trimWhitespace plainTextFile
  expectedOutputFile <- readFile "Challenge5/expected_output.txt"
  let expectedOutput = concat $ lines expectedOutputFile
  xorKeyFile <- readFile "challenge5/xor_key.txt"
  let xorKey = concat $ lines xorKeyFile
  let xorKeyRepeated = repeatToLength (length plainText) xorKey
  let cipherText = xorStrings plainText xorKeyRepeated
  let encryptedString = Hex.encode cipherText
  putStatus("Plaintext: (length " ++ show(length plainText) ++ ") '" ++ plainText ++ "'")
  putStatus("Expected Output: (length " ++ show(length expectedOutput) ++ ") '" ++ expectedOutput ++ "'")
  putStatus("XOR Key: (length " ++ show(length xorKey) ++ ") '" ++ xorKey ++ "'")
  putStatus("Key repeated: '" ++ xorKeyRepeated ++ "'")
  putStatus("Encrypted String: (length " ++ show(length encryptedString) ++ ") '" ++ encryptedString ++ "'")
  if encryptedString == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
