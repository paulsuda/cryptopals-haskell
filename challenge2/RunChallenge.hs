module Challenge2.RunChallenge (main, run) where

main :: IO ()
main = do
  run(putStrLn, putStrLn, putStrLn)


run (putResult, putError, putStatus) = do
  putResult("coming")
  putError("soon")
