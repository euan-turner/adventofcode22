module Main where

import Data.List ( nub )

parseBuffer :: String -> Int -> Int 
parseBuffer input n = n + parseBuffer'  (drop n input) (take n input)
  where 
    parseBuffer' :: String -> String -> Int
    parseBuffer' sig@(c:cs) buf@(b:bs)
      | buf == nub buf = 0
      | otherwise = 1 + parseBuffer' cs (bs ++ [c])

main ::IO ()
main = do 
  input <- readFile "buffer.txt"
  putStrLn(show(parseBuffer input 14))