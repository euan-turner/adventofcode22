module Main where

scores :: [(String, Int)]
scores = [("X", 1), ("Y", 2), ("Z", 3)]


wins :: [[String]]
wins = [["A", "Y"], ["B", "Z"], ["C", "X"]]


draws :: [[String]]
draws = [["A", "X"], ["B", "Y"], ["C", "Z"]]

losses :: [[String]]
losses = [["A", "Z"], ["B", "X"], ["C", "Y"]]

convertScores :: String -> Int 
convertScores input = sum points
  where
    rounds = lines input
    plays = map words rounds
    points = map singleScore plays


    singleScore :: [String] -> Int
    singleScore r@[opp, me]
      | r `elem` wins = 6 + lookUp me scores 
      | r `elem` draws = 3 + lookUp me scores
      | otherwise = lookUp me scores
 
lookUp :: String -> [(String, Int)] -> Int
lookUp k kvs = head [v | (k', v) <- kvs, k' == k]

partTwo :: String -> Int 
partTwo input = sum points
  where 
    rounds = map words (lines input)
    points = map singleScore rounds

    singleScore :: [String] -> Int
    singleScore r@[opp, res]
      | res == "X" = lookUp (getTurn opp losses) scores
      | res == "Y" = 3 + lookUp (getTurn opp draws) scores
      | otherwise = 6 + lookUp (getTurn opp wins) scores

getTurn :: String -> [[String]] -> String
getTurn o ts = head [t | [o', t] <- ts, o == o']

main :: IO ()
main = do
  file <- readFile "rounds.txt"
  putStrLn(show(partTwo file))