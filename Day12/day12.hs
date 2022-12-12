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
      | otherwise = [minimum paths]
      where 
        -- Go up
        up = if succ c <= n then traverse' (x, y - 1) (xy:xys) n ns (c:south) east west else []
        -- Go down
        down = if succ c <= s then traverse' (x, y + 1) (xy:xys) s (c:north) ss east west else []
        -- Go left
        left = if succ c <= e then traverse' (x - 1, y) (xy:xys) e north south es (c:west) else []
        -- Go right
        right = if succ c <= w then traverse' (x + 1, y) (xy:xys) w north south (w:east) ws else []
        paths = up ++ down ++ left ++ right 

traverse2 :: Pos -> [Pos] -> [String] -> [[Pos]]
traverse2 xy@(x, y) xys ls
  | ls !! y !! x == 'E' = [[xy]]
  | xy `elem` xys = []
  | otherwise = concatMap (xy:) (left ++ right ++ up ++ down)
  where 
    c = ls !! y !! x
    width = length (ls !! 0)
    height = length ls
    left = if (x > 0) && (ls !! y !! (x - 1) <= succ c) then traverse2 (x - 1, y) (xy:xys) ls else []
    right = if (x < width) && (ls !! y !! (x + 1) <= succ c) then traverse2 (x + 1, y) (xy:xys) ls else []
    up = if (x > 0) && (ls !! (y - 1) !! x <= succ c) then traverse2 (x, y - 1) (xy:xys) ls else []
    down = if (x < height) && (ls !! (y + 1) !! x <= succ c) then traverse2 (x, y + 1) (xy:xys) ls else []



main :: IO ()
main = do
  input <- readFile "sample.txt"
  let ls = lines input
  let start = findStart ls 0
  let paths = traverse2 start [] ls
  putStrLn(show paths)