module Main where 

import Data.List (transpose)
import Data.Char (digitToInt)

-- don't call with top or bottom rows
visibleRows :: [[Int]] -> [[Int]] -- returns indices of visible trees in each row
visibleRows [] = []
visibleRows (r:rs) = visibleRow r (-1) 0 : visibleRows rs
  where -- use nub to simplify
                  -- tallest so far -- current index
    visibleRow ::[Int] -> Int -> Int -> [Int] -- returns indices of tallest trees
    visibleRow [] _ _ = []
    visibleRow (t:ts) h i
      | t > h = i : visibleRow ts t (i+1)
      | otherwise = visibleRow ts h (i+1)


main :: IO ()
main = do 
  input <- readFile "trees.txt"
  let rowsR = map (\r -> map digitToInt r) (lines input) 
  let colsD = transpose rowsR
  let rowsL = map reverse rowsR 
  let colsU = map reverse colsD
  -- putStrLn(show (visibleRows (tail (init rowsR))))
  putStrLn(show (visibleRows (tail (init rowsL))))

  -- remember to add each border tree at end