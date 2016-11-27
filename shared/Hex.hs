
module Shared.Hex (decode, encode, charVal, valChar) where

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

valChar 0 = '0'
valChar 1 = '1'
valChar 2 = '2'
valChar 3 = '3'
valChar 4 = '4'
valChar 5 = '5'
valChar 6 = '6'
valChar 7 = '7'
valChar 8 = '8'
valChar 9 = '9'
valChar 10 = 'a'
valChar 11 = 'b'
valChar 12 = 'c'
valChar 13 = 'd'
valChar 14 = 'e'
valChar 15 = 'f'

decode :: String -> String
decode "" = ""
decode (a:b:x) = toEnum(charVal a  * 16 + charVal b) : decode x

encode :: String -> String
encode "" = ""
encode (a:x) = [valChar(quot charVal 16)] ++ [valChar(mod charVal 16)] ++ encode x
  where charVal = fromEnum a
