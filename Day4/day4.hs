module Main where

--Pre - char is in string
splitOnChar :: Char -> String -> (String, String)
splitOnChar c s = (first, second)
  where 
    first = takeWhile (/= c) s
    second = drop (length first + 1) s

getSections :: String -> [(String, String)]
getSections input = map (splitOnChar ',') pairs
  where 
    pairs = lines input

countOverlaps :: [(String, String)] -> Int
countOverlaps [] = 0
countOverlaps ((a,b):ps)
  | any (`elem` br) ar = 1 + countOverlaps ps
  | otherwise = countOverlaps ps
  where 
    (as, ae) = splitOnChar '-' a
    (bs, be) = splitOnChar '-' b
    ar = [(read as :: Int)..(read ae :: Int)]
    br = [(read bs :: Int)..(read be :: Int)]

main :: IO ()
main = do
  input <- readFile "pairs.txt"
  let res = (countOverlaps . getSections) input

  putStrLn(show(res))
