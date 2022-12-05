module Main where

import Data.Char ( isDigit, digitToInt ) 
import Data.List (sortBy)
--Pre index is in list
stackLookUp :: Int -> [(Int, [Char])] -> [Char]
stackLookUp i ss = head [cs | (i', cs) <- ss, i == i']

getStacks :: [String] -> [(Int, [Char])]
getStacks input = zip nos stacks
  where 
    stackLines = getStackLines input
    nos = map digitToInt (filter isDigit (last stackLines)) 
    stacks = map (flip buildStack (take (length stackLines - 1) stackLines)) nos

    buildStack :: Int -> [String] -> [Char]
    buildStack i [] = []
    buildStack i (r:rs)
      | box == ' ' = buildStack i rs
      | otherwise = box : buildStack i rs
      where 
        box = r !! (4*i - 3)

getCommands :: [String] -> [[Int]]
getCommands input = map (getCommand . words) (getCommandLines input)
  where 
    -- call with words
    getCommand :: [String] -> [Int]
    getCommand ws = map read (filter (all isDigit) ws) :: [Int]

getStackLines :: [String] -> [String]
getStackLines = takeWhile (/= "")

getCommandLines :: [String] -> [String]
getCommandLines input = drop (length (getStackLines input) + 1) input

executeCommands :: [[Int]] -> [(Int, [Char])] -> [(Int, [Char])]
executeCommands [] ss = ss
executeCommands ([n,f,t]:cs) ss = executeCommands cs (executeCommand n f t ss)
  where 
    executeCommand :: Int -> Int -> Int -> [(Int, [Char])] -> [(Int, [Char])]
    executeCommand 0 _ _ ss = ss
    executeCommand n f t ss = executeCommand (n - 1) f t newSs
      where
        src = stackLookUp f ss  
        dest = stackLookUp t ss  
        remStacks = filter (\s -> s /= (f, src) && s /= (t, dest)) ss
        newDest = (head src : dest)
        newSrc = tail src
        newSs = (t, newDest) : (f, newSrc) : remStacks

getResult :: [[Int]] -> [(Int, [Char])] -> [(Int, [Char])]
getResult is ss = sortBy (\(a,_) (b, _) -> compare a b) (executeCommands is ss)

getTopBox :: [(Int, [Char])] -> [Char]
getTopBox = map (\(_, (c:cs)) -> c)

main :: IO ()
main = do 
  input <- readFile "stacks.txt"
  let ls = lines input
  let stacks = getStacks ls
  let coms = getCommands ls
  let result = getResult coms stacks
  putStrLn (show (getTopBox result))