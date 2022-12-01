module Main where

splitSnacks :: String -> [Int]
splitSnacks input = addTotals (lines input) 0
    where
        addTotals :: [String] -> Int -> [Int]
        addTotals [] acc = [acc]
        addTotals ("":ss) acc = [acc] ++ addTotals ss 0
        addTotals (s:ss) acc = addTotals ss (acc + i)
            where
                i = read s :: Int

getTopThree :: [Int] -> [Int]
getTopThree xs = getTop xs 3
    where
        getTop :: [Int] -> Int -> [Int] 
        getTop _ 0  = []
        getTop xs n = [maximum xs] ++ getTop xs' (n - 1)
                where
                max = maximum xs 
                xs' = filter (/= max) xs

main :: IO ()
main = do 
    file <- readFile "snacks.txt"
    let each = splitSnacks file 

    putStrLn (show (sum (getTopThree each)))