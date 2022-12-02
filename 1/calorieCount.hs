import Data.List.Split
import Data.List
import System.IO

main = do
    elves <- readFile "elves.txt"
    print $ calorieCount elves

calorieCount :: String -> Int
calorieCount counts = sum $ top3 $ map sum $ stringToIntArray counts

top3 :: (Ord a) => [a] -> [a]
top3 list = take 3 $ sortBy (\ a b -> compare b a) list

stringToIntArray :: String -> [[Int]]
stringToIntArray input = map stringListToIntList $ map (splitOn "\n") (splitOn "\n\n" input)

stringListToIntList :: [String] -> [Int]
stringListToIntList strings = map read strings