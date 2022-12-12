module Main where 

type Pos = (Int, Int)

--Find start position
--Build zipper of entire grid
--Find possible next steps
--Call recursively
--Take shortest route
findStart :: [String] -> Int -> Pos
findStart (l:ls) r
  | 'S' `elem` l = (r, findIndex l 0)
  | otherwise = findStart ls (r + 1)
  where 
    findIndex :: String -> Int -> Int
    findIndex (c:cs) col
      | c == 'S' = col 
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
      | otherwise = undefined 
      where 
        -- Go up
        up = if succ c <= n then traverse' (x, y - 1) (xy:xys) n north (n:s:south) (e:east) (w:west)

main :: IO ()
main = do
  input <- readFile "hills.txt"
  let ls = lines input
  let start = findStart ls 0
  putStrLn(show start)