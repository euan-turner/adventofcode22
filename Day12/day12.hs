module Main where 

import Data.Char ( ord )
type Pos = (Int, Int)

--Find start position
--Build zipper of entire grid
--Find possible next steps
--Call recursively
--Take shortest route
findStart :: [String] -> Int -> Pos
findStart (l:ls) r
  | 'S' `elem` l = (r, findIndex l 0)
  | otherwise = findStart ls (r + 1)
  where 
    findIndex :: String -> Int -> Int
    findIndex (c:cs) col
      | c == 'S' = col 
      | otherwise = findIndex cs (col + 1)

traverse :: Pos -> [String] -> Int 
traverse (x, y) ls = undefined 
  where 
    traverse' :: Pos -> [Pos] -> Char -> String -> String -> String -> String -> [Int]
    traverse' xy@(x, y) xys c "" (s:south) (e:east) (w:west) = undefined
    traverse' xy@(x, y) xys c (n:north) "" (e:east) (w:west) = undefined
    traverse' xy@(x, y) xys c (n:north) (s:south) "" (w:west) = undefined 
    traverse' xy@(x, y) xys c (n:north) (s:south) (e:east) "" = undefined
    traverse' xy@(x, y) xys c north@(n:ns) south@(s:ss) east@(e:es) west@(w:ws)
      | xy `elem` xys = []
      | c == 'E' = [length xys]
      | otherwise = [minimum paths]
      where 
        -- Go up
        up = if succ c <= n then traverse' (x, y - 1) (xy:xys) n ns (c:south) east west else []
        -- Go down
        down = if succ c <= s then traverse' (x, y + 1) (xy:xys) s (c:north) ss east west else []
        -- Go left
        left = if succ c <= e then traverse' (x - 1, y) (xy:xys) e north south es (c:west) else []
        -- Go right
        right = if succ c <= w then traverse' (x + 1, y) (xy:xys) w north south (w:east) ws else []
        paths = up ++ down ++ left ++ right 

traverse2 :: Pos -> [Pos] -> [[Int]] -> Int
traverse2 xy@(x, y) xys ls
  | ls !! y !! x == 26 = 0
  | xy `elem` xys = 10000
  | otherwise = 1 + minimum [left, right, up , down]
  where 
    h = ls !! y !! x
    width = length (ls !! 0) - 1
    height = length ls - 1
    left = if (x > 0) && (ls !! y !! (x - 1) <= succ h) then traverse2 (x - 1, y) (xy:xys) ls else 10000
    right = if (x < width) && (ls !! y !! (x + 1) <= succ h) then traverse2 (x + 1, y) (xy:xys) ls else 10000
    up = if (y > 0) && (ls !! (y - 1) !! x <= succ h) then traverse2 (x, y - 1) (xy:xys) ls else 10000
    down = if (y < height) && (ls !! (y + 1) !! x <= succ h) then traverse2 (x, y + 1) (xy:xys) ls else 10000


formatLine :: String -> [Int]
formatLine [] = []
formatLine ('S':cs) = 0 : formatLine cs
formatLine ('E':cs) = 26 : formatLine cs 
formatLine (c:cs) = ord c - ord 'a' : formatLine cs

main :: IO ()
main = do
  input <- readFile "hills.txt"
  let ls = lines input
  let start = findStart ls 0
  let f = map formatLine ls
  let dist = traverse2 start [] f
  putStrLn (show dist)

-- May need to do A*
-- https://www.geeksforgeeks.org/a-search-algorithm/