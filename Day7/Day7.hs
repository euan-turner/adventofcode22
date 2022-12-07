module Main where 


import Data.Char ( isDigit )

type Directory = String
type Size = Int
type SubDirectoryMap = [(Directory, [Directory])]
type DirectoryFileSize = [(Directory, Size)]

-- Returns first occurrence
lookUp :: Directory -> a -> [(Directory, a)] -> a
lookUp k def kvs
  | null fil = def 
  | otherwise = head fil
  where 
    fil = [v | (k', v) <- kvs, k == k']

buildDirectory :: [String] -> DirectoryFileSize -> SubDirectoryMap -> (DirectoryFileSize, SubDirectoryMap)
buildDirectory [] fs sm = (fs, sm)
buildDirectory (('$':' ':'c':'d':' ':dir):ls) fs sm
  | dir == ".." = buildDirectory ls fs sm
  | otherwise = buildDirectory ls fs' sm'
  where 
    fs' = fs ++ [(dir, directoryFileSize ls)]
    sm' = sm ++ [(dir, directorySubdirectories ls)]
buildDirectory (_:ls) fs sm = buildDirectory ls fs sm

-- called after ls, should go until next cd
directoryFileSize :: [String] -> Size
directoryFileSize [] = 0
directoryFileSize (('$':' ':'c':'d':' ':dir):cs)  = 0
directoryFileSize (('d':'i':'r':dir):cs) = directoryFileSize cs
directoryFileSize (file:cs) = (read size :: Size) + directoryFileSize cs
  where 
    size = (head . words) file

directorySubdirectories :: [String] -> [Directory]
directorySubdirectories [] = []
directorySubdirectories (('$':' ':'c':'d':' ':dir):ls) = []
directorySubdirectories (('d':dir):ls) = (last . words) dir : directorySubdirectories ls
directorySubdirectories (_:ls) = directorySubdirectories ls

transitiveSubDirectories :: SubDirectoryMap -> SubDirectoryMap
transitiveSubDirectories [] = []
transitiveSubDirectories ((dir, sdirs):sm) = (dir, sdirs ++ sdirs') : sm'
  where 
    sm' = transitiveSubDirectories sm
    sdirs' = concatMap (\sd -> lookUp sd [] sm') sdirs

transitiveFileSize :: SubDirectoryMap -> DirectoryFileSize -> DirectoryFileSize
transitiveFileSize _ [] = []
transitiveFileSize sm ((dir, size): fs) = (dir, size + size') : fs'
  where 
    fs' = transitiveFileSize sm fs 
    sdirs = lookUp dir [] sm 
    size' = sum (map (\sd -> lookUp sd 0 fs) sdirs)

showSM :: SubDirectoryMap -> String
showSM [] = ""
showSM ((dir, dirs) : sm) = dir ++ ": " ++  show dirs ++ "\n" ++ showSM sm 

showFS :: DirectoryFileSize -> String 
showFS [] = ""
showFS ((dir, size): fs) = dir ++ ": " ++ show size ++ "\n" ++ showFS fs

totalSize :: [String] -> Size 
totalSize [] = 0
totalSize (l@(d:r):ls)
  | isDigit d = (read (head (words l)) :: Size) + totalSize ls
  | otherwise = totalSize ls

main :: IO ()
main = do 
  input <- readFile "files.txt"
  let ls = lines input 
  let ls' = filter (/= "$ ls") ls
  let (fs, sm) = buildDirectory ls' [] []
  let sm' = transitiveSubDirectories sm
  let fs' = transitiveFileSize sm' fs
  putStrLn (showFS (take 10 fs))
  putStrLn (showFS (take 10 fs'))

  -- let less = filter (\(_, s) -> s <= 100000) fs'
  -- let total = sum (map snd less)
  -- putStrLn (show total)