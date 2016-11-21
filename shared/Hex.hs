
module Shared.Hex (decode, charVal) where

charVal :: Char -> Int
charVal '0' = 0
charVal '1' = 1
charVal '2' = 2
charVal '3' = 3
charVal '4' = 4
charVal '5' = 5
charVal '6' = 6
charVal '7' = 7
charVal '8' = 8
charVal '9' = 9
charVal 'a' = 10
charVal 'b' = 11
charVal 'c' = 12
charVal 'd' = 13
charVal 'e' = 14
charVal 'f' = 15

decode :: String -> String
decode "" = ""
decode (a:b:x) = [toEnum(charVal(a) * 16 + charVal(b))] ++ decode(x)
