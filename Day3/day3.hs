module Main where

import Data.Char ( ord, isLower, isUpper )
import Data.List ( nub )

getCompartments :: String -> [(String, String)]
getCompartments input = map (\p -> splitAt ((length p) `div` 2) p) packs
  where
    packs = lines input

getItems :: [(String, String)] -> [String]
getItems [] = []
getItems ((a,b):cs) = [nub (shared a b)] ++ getItems cs

shared :: String -> String -> String
shared [] _ = []
shared (c:cs) b
  | c `elem` b = c : shared cs b
  | otherwise = shared cs b

getPriorities :: [String] -> Int
getPriorities ps = sum all
  where
    all = concatMap (\p -> map convertToPriority p) ps

convertToPriority :: Char -> Int
convertToPriority c 
  | isLower c = ord c - 96
  | isUpper c = ord c - 38

getGroups :: String -> [(String, String, String)]
getGroups input = group packs
  where 
    packs = lines input
    
    group :: [String] -> [(String, String, String)]
    group [] = []
    group (a:b:c:ps) = (a, b, c) : group ps

getBadge :: [(String, String, String)] -> [Char]
getBadge = map getSharedBadge
  where 
    getSharedBadge :: (String, String, String) -> Char
    getSharedBadge (a,b,c) = head abc 
      where
        ab = shared a b
        abc = shared ab c

getBadgePriorities :: [Char] -> Int
getBadgePriorities = (sum . map (convertToPriority))

main :: IO ()
main = do
  file <- readFile "bags.txt"
  let res = (getBadgePriorities . getBadge . getGroups) file
  putStrLn(show(res))