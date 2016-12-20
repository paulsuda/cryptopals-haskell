
module Challenge6.RunChallenge (run, main) where
import qualified Shared.Base64 as Base64
import qualified Shared.Hex as Hex
import Shared.XorUtils (xorStrings)
import Shared.TextUtils (boundedSubString, repeatToLength, trimWhitespace)
import qualified Shared.BitValue as BitValue

keySizeRange :: (Int, Int)
keySizeRange = (2, 40)


main :: IO ()
main = run(putStrLn, putStrLn, putStrLn)

rawEscapeChar :: Char -> Char
rawEscapeChar c
  | n >= 127  = '.'
  | n < 32    = '_'
  | otherwise = c
  where n = fromEnum c

-- Show a string, subbing out nonprintable chars with something else.
rawEscape :: String -> String
rawEscape "" = ""
rawEscape s = thisChar : rawEscape (tail s)
  where thisChar = rawEscapeChar (head s)

hexComboView :: String -> String
hexComboView "" = ""
hexComboView s = " | " ++ rawPart ++ " | " ++ "  " ++ hexPart ++ "\n" ++ hexComboView remainingData
  where lineLength = 16
        rawPart = rawEscape lineData
        hexPart = Hex.encodeSeparated lineData ":"
        lineData = boundedSubString s 0 lineLength
        remainingData = boundedSubString s lineLength (length s - lineLength)


solveRepeatingKey :: String -> String
solveRepeatingKey cipherText = "key coming soon"

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
  putStatus("Plaintext:\n" ++ hexComboView plainText)
  putStatus("Expected Output: (length " ++ show(length expectedOutput) ++ ") '" ++ expectedOutput ++ "'")
  if plainText == expectedOutput then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
