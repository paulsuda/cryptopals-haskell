
module Challenge9.RunChallenge (main, run) where

import Shared.Challenge (ChallengeRunner)
import Shared.Views (hexShow)
import Data.ByteString.Char8 as ByteString

pkcs7pad :: Int -> ByteString -> ByteString
pkcs7pad blockSize s = ByteString.append s padding
  where padCount = if blockStartBytes == 0 then 0 else blockSize - blockStartBytes
        blockStartBytes = mod (ByteString.length s) blockSize
        padChar = toEnum padCount :: Char
        padding = ByteString.replicate padCount padChar

main :: IO ()
main = run Prelude.putStrLn Prelude.putStrLn Prelude.putStrLn

run :: ChallengeRunner
run putResult putError putStatus = do
  testStringsFile <- ByteString.readFile "challenge9/test-strings.txt"
  let testStrings = ByteString.lines testStringsFile
  let blockSize = 4
  let testString = ByteString.pack "abcde"
  let testExpect = ByteString.pack "abcde\03\03\03"
  let fn = hexShow . ByteString.unpack . pkcs7pad blockSize
  let messages = Prelude.map fn testStrings
  putStatus (Prelude.unlines messages)
  let testVal = pkcs7pad blockSize testString
  if testVal == testExpect then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
