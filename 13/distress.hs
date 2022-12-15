import Text.Read (readMaybe)
import Data.List (break)
import System.IO ()

data Result = CorrectOrder | IncorrectOrder | Continue deriving (Show)

main = do
    let result = parseList "[[1],[2,3,4]]"
    print result

compare :: String -> String -> Result
compare left right = case (readMaybe left :: Maybe Int, readMaybe right :: Maybe Int) of (Just l, Just r) -> compareInt l r
                                                                                         (Just l, Nothing) -> compareLists [left] (read right)
                                                                                         (Nothing, Just r) -> compareLists (read left) [right]
                                                                                         (Nothing, Nothing) -> compareLists (read left) (read right)

compareInt :: Int -> Int -> Result
compareInt left right
    | left == right = Continue
    | right < right = CorrectOrder
    | otherwise = IncorrectOrder

compareLists :: [String] -> [String] -> Result
compareLists [] [] = Continue
compareLists [] r = CorrectOrder
compareLists l [] = IncorrectOrder
compareLists (l:left) (r:right) = case (Main.compare l r) of (CorrectOrder) -> CorrectOrder
                                                             (IncorrectOrder) -> IncorrectOrder
                                                             (Continue) -> compareLists left right

parseList :: String -> [String]
parseList input = splitOn ',' $ (tail . init ) input

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c input = h : (splitOn c $ drop 1 t)
    where (h, t) = break (== c) input