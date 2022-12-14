module Main where 

import Data.List (nub)
type Pos = (Int, Int)

-- Input string, lowest y so far (so highest integer), return list of pos
formatPaths :: [String] -> [Pos]
formatPaths [] = []
formatPaths (l:ls) = full ++ paths
  where
    xys = filter (/= "->") (words l)
    xs = map (takeWhile (/= ',')) xys
    ys = map ((tail).(dropWhile (/= ','))) xys
    corners = zipWith (\a -> \b -> (read a :: Int, read b :: Int)) xs ys
    full = (nub.fullPaths) (zip corners (tail corners))
    paths = formatPaths ls 

    fullPaths :: [(Pos, Pos)] -> [Pos]
    fullPaths [] = []
    fullPaths (((x, y), (x', y')):segs)
      | x < x' = zip [x..x'] (repeat y) ++ fullPaths segs -- moving right 
      | x > x' = zip [x'..x] (repeat y) ++ fullPaths segs -- moving left
      | y < y' = zip (repeat x) [y..y'] ++ fullPaths segs -- moving up
      | otherwise = zip (repeat x) [y'..y] ++ fullPaths segs -- moving down
{-
Start with path of rocks
Add grain of sand
Find resting place
Add sand position to path of rocks
Repeat until sand 'falls' of the edge i.e. its y gets below the lowest rock
-}

countSand :: [Pos] -> Int -> Int 
countSand ps n
  | ps == ps' = n -- New grain fell into chasm
  | otherwise = countSand ps' (n+1)
  where 
    source = (500,0)
    bottom = maximum (map snd ps)
    ps' = placeSand source

    placeSand :: Pos -> [Pos]
    placeSand (x, y)
      | y >= bottom = ps -- Use no change to signal end
      | notElem (x, y+1) ps = placeSand (x, y+1)
      | notElem (x-1, y+1) ps = placeSand (x-1, y+1)
      | notElem (x+1, y+1) ps = placeSand (x+1, y+1)
      | otherwise = (x, y) : ps

countSand' :: [Pos] -> Int -> Int -> Int
countSand' ps n ground 
  | elem source ps' = n + 1
  | otherwise = countSand' ps' (n+1) ground
  where 
    source = (500,0)
    ps' = placeSand' ground source

    placeSand' :: Int -> Pos ->[Pos]
    placeSand' ground (x, y)
      | y == ground - 1 = (x, y) : ps
      | notElem (x, y+1) ps = placeSand' ground (x, y+1) 
      | notElem (x-1, y+1) ps = placeSand' ground (x-1, y+1)
      | notElem (x+1, y+1) ps = placeSand' ground (x+1, y+1)
      | otherwise = (x, y) : ps

main :: IO ()
main = do
  input <- readFile "paths.txt"
  let path = formatPaths (lines input)
  let ground = 2 + maximum (map snd path)
  putStrLn (show ground)
  let n = countSand path 0
  putStrLn (show n)
  let n' = countSand' path 0 ground
  putStrLn (show n')
  