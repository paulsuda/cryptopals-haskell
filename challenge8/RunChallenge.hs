
module Challenge8.RunChallenge (main, run) where

import Shared.Challenge (ChallengeRunner)

-- import Shared.Hex as Hex
import Data.Hex as Hex
import Data.ByteString.Char8 as ByteString

main :: IO ()
main = run Prelude.putStrLn Prelude.putStrLn Prelude.putStrLn

cipherTextLines :: ByteString -> [ByteString]
cipherTextLines s = do
  return (Prelude.map Hex.unhex (ByteString.lines s))

run :: ChallengeRunner
run putResult putError putStatus = do
  cipherTextFile <- ByteString.readFile "challenge8/ciphertext.txt"
  -- let cipherTextLines = ByteString.lines cipherTextFile
  let cipherTextData = cipherTextLines cipherTextFile
  -- let cipherTextData = Hex.unhex cipherTextLines

  if False then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
