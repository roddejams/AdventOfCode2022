import System.IO
import Data.List
import Data.Char

main = do
    strategy <- readFile "strategy.txt"
    print $ sum $ map calculateScore $ stringToTuples strategy

calculateScore :: (Char, Char) -> Int
calculateScore (a, b) =  scoreForRound b + (scoreForShape $ rockPaperScissorsChoice a b)

rockPaperScissorsChoice :: Char -> Char -> Char
rockPaperScissorsChoice a 'X' = loses a
rockPaperScissorsChoice a 'Y' = ties a
rockPaperScissorsChoice a 'Z' = beats a

beats :: Char -> Char
beats 'A' = 'B'
beats 'B' = 'C'
beats 'C' = 'A'

ties :: Char -> Char
ties c = c

loses :: Char -> Char
loses 'A' = 'C'
loses 'B' = 'A'
loses 'C' = 'B'

scoreForShape :: Char -> Int
scoreForShape 'A' = 1
scoreForShape 'B' = 2
scoreForShape 'C' = 3

scoreForRound :: Char -> Int
scoreForRound 'X' = 0
scoreForRound 'Y' = 3
scoreForRound 'Z' = 6

stringToTuples :: String -> [(Char, Char)]
stringToTuples input = map stringToTuple $ map (filter (\x -> not $ isSpace x)) $ lines input

stringToTuple :: String -> (Char, Char)
stringToTuple (a:b:xs) = (a, b)