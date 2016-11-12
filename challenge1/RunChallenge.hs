


decodeBase :: String -> String
decodeBase "" = ""
decodeBase (a:b:c:x) = "[" ++ [a] ++ "," ++ [b] ++ "," ++ [c] ++ "]-" ++ decodeBase(x)



main = do
  inp <- readFile "input.txt"
  let inpok = init inp
  let l = show(length(inpok))
  putStrLn("Input (length " ++ l ++ ")")
  putStrLn "Result:"
  putStrLn(decodeBase(inpok))
