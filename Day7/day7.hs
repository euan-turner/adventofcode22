module Main where

import Data.List (minimumBy)

data Use = CD Directory | File Size
  deriving (Show, Eq)
type Directory = String
type Size = Int 
type DirSizes = [(Directory, Size)]
type Path = [Directory]

formatInput :: [String] -> [Use]
formatInput [] = []
formatInput ("$ ls":ls) = formatInput ls 
formatInput (('$':' ':'c':'d':' ':dir):ls) = (CD dir) : formatInput ls 
formatInput (('d':'i':'r':' ':dir):ls) = formatInput ls
formatInput (l:ls) = (File (read size :: Size)) : formatInput ls 
  where 
    [size, name] = words l

buildSizes :: [Use] -> DirSizes -> DirSizes
buildSizes [] dss = dss
buildSizes (CD "..":us) (ds:dss') = ds : buildSizes us dss'
buildSizes (CD dir:us) dss = buildSizes us ((dir, 0):dss)
buildSizes (File size:us) dss = buildSizes us dss'
  where 
    dss' = map (\(d, s) -> (d, s + size)) dss

smallTotal :: DirSizes -> Size 
smallTotal ds = sum (map (\(d, s) -> s) (filter (\(d, s) -> s <= 100000) ds))

toDelete :: DirSizes -> Size 
toDelete dss = snd sup
  where 
    req = head [s | (d, s) <- dss, d == "/"] - 40000000
    largeEnough = filter (\(d, s) -> s >= req) dss
    sup = minimumBy (\(d, s) -> \(d', s') -> compare s s') largeEnough

showSizes :: DirSizes -> String
showSizes [] = ""
showSizes ((d, s):dss) = d ++ " " ++ show s ++ "\n" ++ showSizes dss

main :: IO ()
main = do
  input <- readFile "files.txt"
  let fls = (formatInput . lines) input
  let sizes = buildSizes fls []
  putStrLn (show (toDelete sizes))