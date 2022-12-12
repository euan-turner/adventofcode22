module Main where 

import Data.Char ( ord, toUpper )
import Algorithm.Search ( aStar, dijkstra )
import Data.Maybe ( fromJust )
type Pos = (Int, Int)
type Graph = [(Pos, [Pos])]

findPos :: [String] -> Char -> Int -> Pos
findPos (l:ls) t r
  | t `elem` l = (findIndex l 0, r)
  | otherwise = findPos ls t (r + 1)
  where 
    findIndex :: String -> Int -> Int
    findIndex (c:cs) col
      | c == t = col 
      | otherwise = findIndex cs (col + 1)

findRoute :: [String] -> [[Int]] -> Maybe (Int, [Pos])
findRoute ls hs =  dijkstra (ns hs) tCost (==end) start
  where 
    start = findPos ls 'S' 0
    end = findPos ls 'E' 0

ns :: [[Int]] -> Pos -> [Pos]
ns hs (x, y) = canReach 
  where 
    inMap = filter (\(a, b) -> a >= 0 && a <= length (head hs) - 1 && b >= 0 && b <= length hs - 1) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    canReach = filter (\(a, b) -> hs !! b !! a <= hs !! y !! x + 1) inMap

tCost :: Pos -> Pos -> Int
tCost _ _ = 1

findStart :: [String] -> [[Int]] -> Maybe (Int, [Pos])
findStart ls hs = dijkstra (ns' hs) tCost (\(x, y) -> ls !! y !! x == 'a') start
  where 
    start = findPos ls 'E' 0

ns' :: [[Int]] -> Pos -> [Pos]
ns' hs (x, y) = canReach
  where 
    inMap = filter (\(a, b) -> a >= 0 && a <= length (head hs) - 1 && b >= 0 && b <= length hs - 1) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    canReach = filter (\(a, b) -> hs !! b !! a >= hs !! y !! x - 1) inMap


formatLine :: String -> [Int]
formatLine [] = []
formatLine ('S':cs) = 0 : formatLine cs
formatLine ('E':cs) = 25 : formatLine cs 
formatLine (c:cs) = ord c - ord 'a' : formatLine cs

visualise :: [String] -> [Pos] -> [String]
visualise ls [] = ls 
visualise ls ((x, y):ps) = visualise (replace ls) ps 
  where 
    replace :: [String] -> [String]
    replace l = take y l ++ [(take x (l !! y) ++ [toUpper (l !! y !! x)] ++ drop (x+1) (l !! y))] ++ drop (y+1) l

main :: IO ()
main = do
  input <- readFile "hills.txt"
  let ls = lines input
  let f = map formatLine ls
  
  {-
  Part One
  let res = findRoute ls f 
  let (n, r) = fromJust res
  print n
  let vis = visualise ls r
  mapM_ print vis
  -}
  let res = findStart ls f 
  let (n, r) = fromJust res 
  print n 
  let vis = visualise ls r 
  mapM_ print vis

--528
--522