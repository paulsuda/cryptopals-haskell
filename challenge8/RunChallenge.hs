
module Challenge8.RunChallenge (main, run) where

import Shared.Challenge (ChallengeRunner)

import Shared.Histogram (HistValue, HistArray, histMin, histMax)
import Shared.Views (histShow)

-- import Shared.Hex as Hex
import Data.Hex as Hex
import Data.ByteString.Char8 as ByteString
import Data.List (nub)
import Data.List.Split (chunksOf)

main :: IO ()
main = run Prelude.putStrLn Prelude.putStrLn Prelude.putStrLn

cipherTextLines :: ByteString -> [ByteString]
cipherTextLines s = Prelude.head dataLines
  where lines = ByteString.lines s
        dataLines = Prelude.mapM Hex.unhex lines

chunkData :: ByteString -> [ByteString]
chunkData xs = ByteString.take blockSize xs : remaining
  where blockSize = 16
        remaining = if ByteString.length xs > blockSize
                      then chunkData (ByteString.drop blockSize xs)
                      else []

mostRepeatCount :: ByteString -> Int
mostRepeatCount s = chunksLength - uniqueChunksLength
  where uniqueChunksLength = Prelude.length uniqueChunks
        uniqueChunks = nub chunks
        chunksLength = Prelude.length chunks
        chunks = chunkData s

mostRepeatsHist :: [ByteString] -> HistArray
mostRepeatsHist [] = []
mostRepeatsHist dataLines = fromIntegral (mostRepeatCount (Prelude.head dataLines)) : mostRepeatsHist (Prelude.tail dataLines)

run :: ChallengeRunner
run putResult putError putStatus = do
  cipherTextFile <- ByteString.readFile "challenge8/ciphertext.txt"
  let cipherTextData = cipherTextLines cipherTextFile
  let scoreHist = mostRepeatsHist cipherTextData
  let (maxIndex, maxScore) = histMax scoreHist
  putStatus(histShow scoreHist ++ "Found item index " ++ show maxIndex ++ " with score " ++ show maxScore)
  -- Maybe 132 is correct, not sure??
  if maxIndex == 132 then putResult "OK! Expected result."
  else putError "ERROR! Result not as expected."
