module Main where 

import Data.List (transpose)
import Data.Char (digitToInt)

-- don't call with top or bottom rows
visibleRows :: [[Int]] -> [[Int]] -- returns indices of visible trees in each row
visibleRows [] = []
visibleRows (r:rs) = visibleRow r (-1) 0 : visibleRows rs
  where -- use nub to simplify, then can takeWhile
        -- then can get first index of each occurrence :: [Int] -> [Int] -> [Int]
        -- that is row -> elems to look for -> elem indices
                  -- tallest so far -- current index
    visibleRow ::[Int] -> Int -> Int -> [Int] -- returns indices of tallest trees
    visibleRow [] _ _ = []
    visibleRow (t:ts) h i
      | t > h = i : visibleRow ts t (i+1)
      | otherwise = visibleRow ts h (i+1)

-- for each row
-- for each tree, check if it is tallest in rest of row, or get associated section of column up to it (use row index)
-- that is, for rowsR, it it can be seen from the right
-- check all less than
-- don't call with top or bottom row,just add borders at end
visible :: [[Int]] -> [[Int]] -> Int -> Int 
visible [] _ _ = 0
visible (r:rs) cs ri = visible' [head r] (tail r) 1 + visible rs cs (ri + 1)
  where 
    visible' :: [Int] -> [Int] -> Int -> Int 
    visible' _ [t] _ = 0 -- don't cant last column
    visible' ts' (t:ts) ci
      | all (< t) ts' || all (< t) ts = 1 + visible' (ts' ++ [t]) ts (ci + 1)
      | all (< t) above || all (< t) below = 1 + visible' (ts' ++ [t]) ts (ci + 1)
      | otherwise = visible' (ts' ++ [t]) ts (ci + 1)
      where 
        col = cs !! ci 
        above = take ri col 
        below = drop (ri + 1) col

totalVisible :: [[Int]] -> [[Int]] -> Int 
totalVisible rows cols = 
  visible (tail (init rows)) cols 1 + (4 * length rows - 4)

scenic :: [[Int]] -> [[Int]] -> Int -> [[Int]]
scenic [] _ _  = []
scenic (r:rs) cs ri = scenic' [head r] (tail r) 1 : scenic rs cs (ri + 1)
  where 
    scenic' :: [Int] -> [Int] -> Int -> [Int]
    scenic' _ [t] _ = [] 
    scenic' ts' (t:ts) ci = score : scenic' (t:ts') ts (ci + 1) -- count left, right, up, down
      where 
        col = cs !! ci 
        above = reverse (take ri col) 
        below = drop (ri + 1) col
        score = product (map (seen t) [ts', ts, above, below])
seen :: Int -> [Int] -> Int
seen t ts
  | s /= length ts = 1 + s
  | otherwise = s
  where
    s = length (takeWhile (< t) ts)

bestScenic :: [[Int]] -> [[Int]] -> Int 
bestScenic rs cs = (maximum . concat) (scenic (tail (init rs)) cs 1)

main :: IO ()
main = do 
  input <- readFile "trees.txt"
  let rows = map (\r -> map digitToInt r) (lines input) 
  let cols = transpose rows
  -- let rowsL = map reverse rowsR 
  -- let colsU = map reverse colsD
  -- putStrLn(show (visibleRows (tail (init rowsR))))
  -- let total = totalVisible rows cols
  let scenic = bestScenic rows cols
  putStrLn(show scenic)

  -- remember to add each border tree at end