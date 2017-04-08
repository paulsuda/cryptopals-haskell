
module Challenge7.RunChallenge (main, run) where

import Shared.Challenge (ChallengeRunner)
import Shared.Base64 as Base64

import qualified Data.ByteString.Char8
-- import qualified Data.ByteString.Lazy as L
-- import Data.ByteString (ByteString)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
import Crypto.Error (throwCryptoError, CryptoFailable(..))

main :: IO ()
main = run putStrLn putStrLn putStrLn

type Key = Data.ByteString.Char8.ByteString

secretKey :: Key
-- secretKey = Data.ByteString.Char8.pack "12-456-89A-CDE-012-456-89A-CDE-"
secretKey = Data.ByteString.Char8.pack "YELLOW SUBMARINE"


cipherInitNoErr :: BlockCipher c => Key -> c
cipherInitNoErr k = case cipherInit k of
  CryptoPassed a -> a
  CryptoFailed e -> error (show e)

-- cipherMakeKey :: Cipher cipher => cipher -> Key -> cipher
-- cipherMakeKey _ = Key -- Yeah Lazyness!!!!!!

run :: ChallengeRunner
run putResult putError putStatus = do
  cipherTextFile <- readFile "challenge7/ciphertext.txt"
  let cipherText = Data.ByteString.Char8.pack $ Base64.decode cipherTextFile
  let ctx = cipherInitNoErr secretKey :: AES128
  let plainText = (ecbDecrypt ctx) cipherText
  Data.ByteString.Char8.putStrLn plainText

  if False then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
