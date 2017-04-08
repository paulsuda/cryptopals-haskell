
module Shared.Challenge (ChallengeRunner) where

type ChallengeRunner = (String -> IO ()) -> (String -> IO ()) -> (String -> IO ()) -> IO ()
