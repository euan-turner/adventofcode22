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

main :: IO ()
main = do 
  input <- readFile "moves.txt"
  let fls = (formatInput . lines) input
  let visited = move fls [] (0, 0) (0, 0)
  -- putStrLn (show (reverse visited))
  putStrLn (show (length visited))