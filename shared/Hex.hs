
module Shared.Hex (decodeHex) where

hexVal :: Char -> Int
hexVal '0' = 0
hexVal '1' = 1
hexVal '2' = 2
hexVal '3' = 3
hexVal '4' = 4
hexVal '5' = 5
hexVal '6' = 6
hexVal '7' = 7
hexVal '8' = 8
hexVal '9' = 9
hexVal 'a' = 10
hexVal 'b' = 11
hexVal 'c' = 12
hexVal 'd' = 13
hexVal 'e' = 14
hexVal 'f' = 15

decodeHex :: String -> String
decodeHex "" = ""
decodeHex (a:b:x) = [toEnum(hexVal(a) * 16 + hexVal(b))] ++ decodeHex(x)
