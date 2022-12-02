import Data.List.Split
import System.IO

main = do
    elves <- readFile "elves.txt"
    print $ calorieCount elves

calorieCount :: String -> Int
calorieCount counts = maximum $ map sum $ stringToIntArray counts

stringToIntArray :: String -> [[Int]]
stringToIntArray input = map stringListToIntList $ map (splitOn "\n") (splitOn "\n\n" input)

stringListToIntList :: [String] -> [Int]
stringListToIntList strings = map read strings