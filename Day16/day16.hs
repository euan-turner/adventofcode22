module Main where 

import Data.Char ( isUpper, isDigit )
type Graph = [(String, (Int, [String]))]

formatInput :: [String]  -> Graph 
formatInput [] = []
formatInput (l:ls) = (head valves, (rate, tail valves)) : formatInput ls 
  where 
    valves = (takeWhile (not.null) . map (take 2) . iterate (drop 2)) (tail (filter isUpper l))
    rate = read (filter isDigit l) :: Int

lookUp :: String -> Graph -> (Int, [String]) 
lookUp k g = head [v | (k', v) <- g, k == k']

release :: Graph -> [String] -> Int -> Int -> String -> Int 
release g open time totalRate node 
  | time >= 30 = totalRate
  | node `elem` open = totalRate + moveNow
  | otherwise = totalRate + max moveNow moveAfter
  where 
    (rate, ns) = lookUp node g 
    moveNow = maximum (map (release g open (time+1) totalRate) ns)
    moveAfter = maximum (map (release g (node:open) (time+2) (totalRate + rate)) ns)

{-
At a node, look for which node to walk to and open (i.e. multiply rate by time remaining once reached)
Move to node, and repeat
Will probably want to take maximum to find best
Will need Dijsktra to find distance to every node
-}

main :: IO () 
main = do 
  input <- readFile "sample.txt"
  let graph = (formatInput . lines) input
  let (aa, (n, ns)) = head graph
  let next = lookUp (head ns) graph
  let pressure = release graph [] 1 0 aa 
  putStrLn(show pressure)
