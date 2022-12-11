module Main where

import Data.Char ( digitToInt, isSpace, isDigit )
-- Id :: Int, Items :: [Int], Op :: (Int -> Int), Test :: Int, True :: Int, False :: Int, timesInspected
data Monkey = Monkey Int [Int] (Int -> Int) Int Int Int Int


formatInput :: [String] -> [Monkey]
formatInput [] = []
formatInput ("":ls) = getMonkey ls : formatInput rest
  where 
    rest = dropWhile (/= "") ls
formatInput ls = getMonkey ls : formatInput rest 
  where 
    rest = dropWhile (/= "") ls

getMonkey :: [String] -> Monkey
getMonkey m = Monkey id items op test true false 0
  where
    relevant = takeWhile (/= "") m 
    id = getId m
    items = getItems m
    op = getOp m 
    test = getTestTrueFalse m 3
    true = getTestTrueFalse m 4
    false = getTestTrueFalse m 5

getId :: [String] -> Int
getId = (digitToInt  . head . last . words . head)

getItems :: [String] -> [Int]
getItems m = map (\i -> read i :: Int) nums 
  where
    line = m !! 1
    nums = words (filter (\c -> isDigit c || isSpace c) line)

getOp :: [String] -> (Int -> Int)
getOp m
  | args !! 1 == "+" = (+ (read (head args) :: Int))
  | args !! 1 == "*" && head args == "old" = (^ 2)
  | otherwise = (* (read (head args) :: Int))
  where 
    line = m !! 2
    args = take 3 ((reverse . words) line)

getTestTrueFalse :: [String] -> Int -> Int 
getTestTrueFalse m i = read ((last . words) line) :: Int
  where 
    line = m !! i

monkeyBiz :: [Monkey] -> [Monkey]
monkeyBiz monkeys = undefined 
  where
    -- remaining monkeys this round, all updated monkeys this round, updated monkeys this round
    round :: [Monkey] -> [Monkey ] -> [Monkey] -- don't recurse over monkeys, recurse over their ids and lookup
    round [] ms = ms
    round (m:ms) ms' = undefined -- need to replaceMonkey in both ms and ms'
      where 
        (newM, toMove) = inspect m
        -- need to replaceMonkey with newM ms'
        -- returns [(monkey id, items)] to move, and updated monkey
        inspect :: Monkey -> (Monkey, [(Int, Int)])
        inspect m@(Monkey id [] op tst tr fa n) = (m, [])
        inspect (Monkey id its op tst tr fa n)
          | worry `mod` tst == 0 = (m, (tr, worry) : r)
          | otherwise = (m, (fa, worry) : r)
          where 
            item = head its 
            worry = (op item) `div` 3
            (m, r) = inspect (Monkey id (tail its) op tst tr fa (n+1))
        
        moveItems :: [Monkey] -> [(Int, Int)] -> [Monkey]
        moveItems = undefined

replaceMonkey :: Monkey -> [Monkey] -> [Monkey]
replaceMonkey m@(Monkey id _ _ _ _ _ _) (m'@(Monkey id' _ _ _ _ _ _):ms)
  | id == id' = m:ms 
  | otherwise = m' : replaceMonkey m ms

main :: IO ()
main = do
  input <- readFile "monkeys.txt"
  let ls = lines input 
  let ms = formatInput ls
  putStrLn ("")