
module Challenge7.RunChallenge (main, run) where

import Shared.Challenge (ChallengeRunner)
import Shared.Base64 as Base64

import qualified Data.ByteString.Char8 as ByteString
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
import Crypto.Error (throwCryptoError, CryptoFailable(..))

main :: IO ()
main = run putStrLn putStrLn putStrLn

type KeyString = ByteString.ByteString

secretKey :: KeyString
secretKey = ByteString.pack "YELLOW SUBMARINE"

cipherInitNoErr :: BlockCipher c => KeyString -> c
cipherInitNoErr k = case cipherInit k of
  CryptoPassed a -> a
  CryptoFailed e -> error (show e)

run :: ChallengeRunner
run putResult putError putStatus = do
  expectedPlainText <- readFile "challenge7/expected.txt"
  cipherTextFile <- readFile "challenge7/ciphertext.txt"
  let cipherText = ByteString.pack $ Base64.decode $ concat $ lines cipherTextFile
  let ctx = cipherInitNoErr secretKey :: AES128
  let plainText = (ecbDecrypt ctx) cipherText
  -- ByteString.writeFile "challenge7/plaintext.txt" plainText
  putStatus $ ByteString.unpack plainText
  if (ByteString.pack expectedPlainText) == plainText then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
