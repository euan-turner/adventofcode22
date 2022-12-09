module Main where

data Move = U Int | D Int | L Int | R Int
  deriving (Show)
type Pos = (Int, Int)
type Vec = (Int, Int)

up :: Vec
up = (0, 1)
down :: Vec
down = (0, -1)
right :: Vec
right = (1, 0)
left :: Vec
left = (-1, 0)

formatInput :: [String] -> [Move] 
formatInput [] = []
formatInput (('U':' ':d):ls) = U (read d :: Int) : formatInput ls
formatInput (('D':' ':d):ls) = D (read d :: Int) : formatInput ls 
formatInput (('L':' ':d):ls) = L (read d :: Int) : formatInput ls 
formatInput (('R':' ':d):ls) = R (read d :: Int) : formatInput ls  

-- head, tail, vector
getVector :: Pos -> Pos -> Vec
getVector (x, y) (x', y') = (signum (x - x'), signum (y - y'))

posPlusVector :: Pos -> Vec -> Pos
posPlusVector (x, y) (dx, dy) = (x + dx, y + dy)

-- Moves left to process, positions the tail has visited, tail pos, head, pos -> list of results
move :: [Move] -> [Pos] -> Pos -> Pos -> [Pos]
move [] hist tail _ 
  | tail `elem` hist = hist 
  | otherwise = tail : hist
move ((U 0):ms) hist tail head
  | tail `elem` hist = move ms hist tail head
  | otherwise = move ms (tail : hist) tail head
move ((D 0):ms) hist tail head
  | tail `elem` hist = move ms hist tail head
  | otherwise = move ms (tail : hist) tail head
move ((L 0):ms) hist tail head
  | tail `elem` hist = move ms hist tail head
  | otherwise = move ms (tail : hist) tail head
move ((R 0):ms) hist tail head
  | tail `elem` hist = move ms hist tail head
  | otherwise = move ms (tail : hist) tail head
move ((U n):ms) hist tail head 
  | newTail `elem` hist = move (U (n-1):ms) hist newTail newHead
  | otherwise = move (U (n-1):ms) (newTail:hist) newTail newHead
  where 
    newHead = posPlusVector head up
    newTail = updateTail newHead tail
move ((D n):ms) hist tail head
  | newTail `elem` hist = move (D (n-1):ms) hist newTail newHead
  | otherwise = move (D (n-1):ms) (newTail:hist) newTail newHead
  where 
    newHead = posPlusVector head down
    newTail = updateTail newHead tail
move ((L n):ms) hist tail head
  | newTail `elem` hist = move (L (n-1):ms) hist newTail newHead
  | otherwise = move (L (n-1):ms) (newTail:hist) newTail newHead
  where 
    newHead = posPlusVector head left
    newTail = updateTail newHead tail
move ((R n):ms) hist tail head 
  | newTail `elem` hist = move (R (n-1):ms) hist newTail newHead
  | otherwise = move (R (n-1):ms) (newTail:hist) newTail newHead
  where 
    newHead = posPlusVector head right
    newTail = updateTail newHead tail

moves :: [Move] -> [Pos] -> [Pos] -> Pos -> [Pos]
moves [] hist knots head
  | (last knots) `elem` hist = hist
  | otherwise = (last knots) : hist
moves ((U 0):ms) hist knots head 
  | (last knots) `elem` hist = moves ms hist knots head 
  | otherwise = moves ms (last knots : hist) knots head
moves ((D 0):ms) hist knots head 
  | (last knots) `elem` hist = moves ms hist knots head 
  | otherwise = moves ms (last knots : hist) knots head
moves ((L 0):ms) hist knots head 
  | (last knots) `elem` hist = moves ms hist knots head 
  | otherwise = moves ms (last knots : hist) knots head
moves ((R 0):ms) hist knots head 
  | (last knots) `elem` hist = moves ms hist knots head 
  | otherwise = moves ms (last knots : hist) knots head
moves ((U n):ms) hist knots head 
  | (last newKnots) `elem` hist = moves (U (n-1):ms) hist newKnots newHead 
  | otherwise = moves (U (n-1):ms) (last newKnots:hist) newKnots newHead
  where 
    newHead = posPlusVector head up
    newKnots = updateKnots newHead knots
moves ((D n):ms) hist knots head 
  | (last newKnots) `elem` hist = moves (D (n-1):ms) hist newKnots newHead 
  | otherwise = moves (D (n-1):ms) (last newKnots:hist) newKnots newHead
  where 
    newHead = posPlusVector head down
    newKnots = updateKnots newHead knots
moves ((L n):ms) hist knots head 
  | (last newKnots) `elem` hist = moves (L (n-1):ms) hist newKnots newHead 
  | otherwise = moves (L (n-1):ms) (last newKnots:hist) newKnots newHead
  where 
    newHead = posPlusVector head left
    newKnots = updateKnots newHead knots
moves ((R n):ms) hist knots head 
  | (last newKnots) `elem` hist = moves (R (n-1):ms) hist newKnots newHead 
  | otherwise = moves (R (n-1):ms) (last newKnots:hist) newKnots newHead
  where 
    newHead = posPlusVector head right
    newKnots = updateKnots newHead knots

-- head, tail (check if tail is 2 away from head)
tailNeedsUpdate :: Pos -> Pos -> Bool
tailNeedsUpdate (hx, hy) (tx, ty) = max dx dy > 1
  where 
    dx = abs (hx - tx)
    dy = abs (hy - ty)
  
updateTail :: Pos -> Pos -> Pos 
updateTail h t
  | tailNeedsUpdate h t = posPlusVector t (getVector h t)
  | otherwise = t

updateKnots :: Pos -> [Pos] -> [Pos]
updateKnots head [] = []
updateKnots head ks@(k:ks')
  | tailNeedsUpdate head k = updateTail head k : updateKnots (updateTail head k) ks'
  | otherwise = ks

main :: IO ()
main = do 
  input <- readFile "moves.txt"
  let fls = (formatInput . lines) input
  let visited = moves fls [] (take 9 (repeat (0,0))) (0, 0)
  putStrLn (show (length visited))