import System.IO
import Data.List
import Data.Char

main = do
    strategy <- readFile "strategy.txt"
    print $ sum $ map calculateScore $ stringToTuples strategy


calculateScore :: (Char, Char) -> Int
calculateScore (a, b) = rockPaperScissorsScore a b + scoreForShape b

rockPaperScissorsScore :: Char -> Char -> Int
rockPaperScissorsScore 'A' b
    | b == 'X' = 3
    | b == 'Y' = 6
    | b == 'Z' = 0

rockPaperScissorsScore 'B' b
    | b == 'X' = 0
    | b == 'Y' = 3
    | b == 'Z' = 6

rockPaperScissorsScore 'C' b
    | b == 'X' = 6
    | b == 'Y' = 0
    | b == 'Z' = 3

scoreForShape :: Char -> Int
scoreForShape 'X' = 1
scoreForShape 'Y' = 2
scoreForShape 'Z' = 3

stringToTuples :: String -> [(Char, Char)]
stringToTuples input = map stringToTuple $ map (filter (\x -> not $ isSpace x)) $ lines input

stringToTuple :: String -> (Char, Char)
stringToTuple (a:b:xs) = (a, b)