module Main where 

import Data.Char ( ord )
import Algorithm.Search ( aStar )
type Pos = (Int, Int)
type Graph = [(Pos, [Pos])]

findPos :: [[Int]] -> Int -> Int -> Pos
findPos (l:ls) t r
  | t `elem` l = (r, findIndex l 0)
  | otherwise = findPos ls t (r + 1)
  where 
    findIndex :: [Int] -> Int -> Int
    findIndex (c:cs) col
      | c == t = col 
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

genGraph :: [[Int]] -> Pos -> Graph
genGraph hs n@(x, y)
  | y == length hs = []
  | otherwise = (n, canReach) : genGraph hs nxt 
  where
    inMap = filter (
      \(a, b) -> a >= 0 && a <= length (head hs) - 1 && b >= 0 && b <= length hs - 1
      ) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    canReach = filter (\(a, b) -> hs !! b !! a <= hs !! y !! x + 1) inMap
    nxt = if (x+1) < (length.head) hs then (x+1, y) else (0, y+1)

findRoutes :: [[Int]] -> (Int, [Pos])
findRoutes hs =  
  where
    start = findPos hs (-1) 0
    (xt, yt) = findPos hs 26 0
    ns :: Pos -> [Pos]
    ns (x, y) = canReach 
      where 
        inMap = filter (\(a, b) -> a >= 0 && a <= length (head hs) - 1 && b >= 0 && b <= length hs - 1) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        canReach = filter (\(a, b) -> hs !! b !! a <= hs !! y !! x + 1) inMap
    tCost :: Pos -> Pos -> Int
    tCost _ _ = 1
    rCost :: Pos -> Int
    rCost (x, y) = abs (xt - x) + abs (yt - y)

formatLine :: String -> [Int]
formatLine [] = []
formatLine ('S':cs) = -1 : formatLine cs
formatLine ('E':cs) = 26 : formatLine cs 
formatLine (c:cs) = ord c - ord 'a' : formatLine cs

main :: IO ()
main = do
  input <- readFile "sample.txt"
  let ls = lines input
  let f = map formatLine ls
  let graph = genGraph f (0,0)
  mapM_ print graph
