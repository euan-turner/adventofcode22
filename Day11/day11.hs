module Main where

import Data.Char ( digitToInt, isSpace, isDigit )
import Data.List ( sort )
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
monkeyBiz monkeys = last (take 10001 (iterate (round 0) monkeys))
  where
    num = length monkeys
    -- current monkey id, current monkeys, updated monkeys
    round :: Int -> [Monkey ] -> [Monkey] -- don't recurse over monkeys, recurse over their ids and lookup
    round n ms
      | n == num = ms
      | otherwise = round (n+1) ms''
      where 
        currentM = lookUpMonkey n ms 
        (newM, toMove) = inspect currentM
        ms' = replaceMonkey newM ms
        ms'' = moveItems ms' toMove
        inspect :: Monkey -> (Monkey, [(Int, Int)])
        inspect m@(Monkey id [] op tst tr fa n) = (m, [])
        inspect (Monkey id its op tst tr fa n)
          | worry `mod` tst == 0 = (m, (tr, worry) : r)
          | otherwise = (m, (fa, worry) : r)
          where 
            item = head its 
            -- worry = (op item) `div` 3
            worry = op item `mod` 9699690 -- product of all divisors, modulo arithmetic
            (m, r) = inspect (Monkey id (tail its) op tst tr fa (n+1))
        moveItems :: [Monkey] -> [(Int, Int)] -> [Monkey]
        moveItems monks [] = monks 
        moveItems monks ((i, v):ps) = moveItems (replaceMonkey newMonk monks) ps
          where
            (Monkey id its op tst tr fa n) = lookUpMonkey i monks 
            newMonk = Monkey id (its ++ [v]) op tst tr fa n

replaceMonkey :: Monkey -> [Monkey] -> [Monkey]
replaceMonkey m@(Monkey id _ _ _ _ _ _) (m'@(Monkey id' _ _ _ _ _ _):ms)
  | id == id' = m:ms 
  | otherwise = m' : replaceMonkey m ms

lookUpMonkey :: Int -> [Monkey] -> Monkey
lookUpMonkey id ((m@(Monkey id' _ _ _ _ _ _)):ms)
  | id == id' = m 
  | otherwise = lookUpMonkey id ms

mostActiveProduct :: [Monkey] -> Int 
mostActiveProduct ms = product (take 2 ns')
  where 
    ns = map (\(Monkey _ _ _ _ _ _ n) -> n) ms
    ns' = (reverse . sort) ns

main :: IO ()
main = do
  input <- readFile "monkeys.txt"
  let ls = lines input 
  let ms = formatInput ls
  let res = monkeyBiz ms 
  mapM_ (\(Monkey id its _ _ _ _ n) -> print (show id ++ ":" ++ show n)) res
  print (show (mostActiveProduct res))