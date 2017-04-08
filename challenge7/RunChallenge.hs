module Challenge7.RunChallenge (main, run) where

-- https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-Tutorial.html
--
-- import Data.ByteString (ByteString)
--
-- import Crypto.Cipher.AES (AES128)
-- import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
-- import Crypto.Error (CryptoFailable(..))

-- secretKey :: ByteString
-- secretKey = "012-456-89A-CDE-012-456-89A-CDE-"

--
-- secretKey :: ByteString
-- secretKey = "YELLOW SUBMARINE" :: ByteString

main :: IO ()
main = run putStrLn putStrLn putStrLn


run putResult putError putStatus = do

  cipherTextFile <- readFile "challenge7/ciphertext.txt"
  -- let plainText = trimWhitespace plainTextFile
  if False then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
