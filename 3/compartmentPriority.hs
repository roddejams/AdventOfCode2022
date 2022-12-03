import System.IO
import Data.Char
import Data.List

main = do
    compartments <- readFile "compartments.txt"
    print $ sum $ map priorityOfBadge $ groupsOf3 $ lines compartments

groupsOf3 :: [String] -> [(String, String, String)]
groupsOf3 [] = []
groupsOf3 (a:b:c:xs) = [(a, b, c)] ++ groupsOf3 xs

priorityOfBadge :: (String, String, String) -> Int
priorityOfBadge (x, y, z) = priority $ head $ intersect x $ intersect y z

priorityOfSharedItem :: String -> Int
priorityOfSharedItem c = priority $ head $ intersect (firstHalfOfList c) (lastHalfOfList c)

priority :: Char -> Int
priority c
    | c <= 'Z'  = ord c - 38
    | c <= 'z'  = ord c - 96
    | otherwise = 0

-- Assume list has even length
firstHalfOfList :: [a] -> [a]
firstHalfOfList x = take ((length x) `div` 2) x

lastHalfOfList :: [a] -> [a]
lastHalfOfList y = drop ((length y) `div` 2) y