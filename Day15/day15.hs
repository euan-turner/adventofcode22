module Main where 

import Data.List ( findIndices )
type Pos = (Int, Int)

format :: [String] -> [(Pos, Pos)]
format [] = []
format (l:ls) = ((ints !! 0, ints !! 1), (ints !! 2, ints !! 3)) : format ls
  where 
    eqs = findIndices (=='=') l 
    nums = map (\i -> takeWhile (\c -> c `notElem` ",:") (drop (i+1) l)) eqs
    ints = map (read) nums :: [Int]

xBounds :: [(Pos, Pos)] -> (Int, Int)
xBounds (((x, y), (x', y')):[]) = (min x x', max x x')
xBounds (((x, y), (x', y')):xys) = (min minX min', max maxX max') 
  where 
    (min', max') = xBounds xys 
    minX = min x x'
    maxX = max x x'

taxiDist :: Pos -> Pos -> Int 
taxiDist (x, y) (x', y') = abs (x - x') + abs (y - y')

checkRow :: [(Pos, Pos)] -> Int -> Int -> Int -> Int
checkRow sbs minX maxX y = length (filter (`notElem` bs) ps')
  where 
    xs = [minX..maxX]
    ps = map (\x -> (x, y)) xs
    ps' = filter (cannotContain sbs) ps
    bs = map snd sbs
-- True if position cannot contain a beacon (i.e. is within zone of a sensor-beacon pair)
cannotContain :: [(Pos, Pos)] -> Pos -> Bool 
cannotContain [] _  = False
cannotContain ((s, b):ps) p = 
  taxiDist s p <= taxiDist s b || cannotContain ps p

findBeacon :: Int -> Int -> [(Pos, Pos)] -> Pos
findBeacon min max sbs = head ((filter (not . cannotContain sbs)) toCheck)
  where 
    toCheck = do 
      x <- [min..max]
      y <- [min..max]
      return (x, y)

-- Part 1
-- main :: IO ()
-- main = do
--   input <- readFile "beacons.txt"
--   let f = format (lines input)
--   let (minX, maxX) = xBounds f
--   let maxDist = maximum (map (uncurry taxiDist) f)
--   let n = checkRow f (minX - maxDist) (maxX + maxDist) 2000000
--   print n
  

-- Part 2
-- 0 <= x,y <= 4000000 (0 and 20 for sample)
-- Multiple x and y
main :: IO ()
main = do
  input <- readFile "beacons.txt"
  let f = format (lines input)
  print (findBeacon 0 4000000 f)