module Main where 

import Data.Char ( digitToInt ) 
import Data.List (intercalate )
data Com = Noop | Addx Int
  deriving (Show)
-- Noop - 1 cycle
-- Addx - 2 cycles

formatInput :: [String] -> [Com]
formatInput [] = []
formatInput (('n':c):ls) = Noop : formatInput ls 
formatInput (('a':'d':'d':'x':' ':n):ls) = Addx dig : formatInput ls 
  where
    dig = read n :: Int 

-- clock cycle, X value, commands, important values
trackX :: Int -> Int -> [Com] -> Int 
trackX c x [] = 0
trackX c x (Noop:cs) 
  | mod (c + 1 + 20) 40 == 0 = x * (c + 1) + trackX (c + 1) x cs
  | otherwise = trackX (c + 1) x cs 
trackX c x (Addx n:cs)
  | mod (c + 1 + 20) 40 == 0 = x * (c + 1) + trackX (c + 2) (x + n) cs 
  | mod (c + 2 + 20) 40 == 0 = x * (c + 2) + trackX (c + 2) (x + n) cs
  | otherwise = trackX (c + 2) (x + n) cs

xTimes :: Int -> Int -> [Com] -> [(Int, Int)]
xTimes c x [] = []
xTimes c x (Noop:cs) = (c, x) : xTimes (c + 1) x cs 
xTimes c x (Addx n:cs) = (c, x) : (c + 1, x) : xTimes (c + 2) (x + n) cs

crt :: [(Int, Int)] -> [String]
crt [] = []
crt ps = crt' row : crt (drop 40 ps)
  where 
    row = take 40 ps
    crt' :: [(Int, Int)] -> String 
    crt' [] = ""
    crt' ((p, x):r)
      | abs ((p `mod` 40) - x) <= 1 = '#' : crt' r 
      | otherwise = '.' : crt' r

main :: IO ()
main = do 
  input <- readFile "coms.txt"
  let fcs = formatInput (lines input)
  --let imp = trackX 0 1 fcs
  let times = xTimes 0 1 fcs
  let screen = crt times
  mapM_ print screen
  
