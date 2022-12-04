import System.IO
import Data.List.Split
import Data.List

main = do
    allSections <- readFile "sections.txt"
    print $ length $ filter listsOverlap $ map stringToFullSections $ lines allSections

-- bad haskell, these should take 2 arguments instead of a tuple so they can be curried
listsOverlap :: (Eq a) => ([a], [a]) -> Bool
listsOverlap (a, b) = any (`elem` a) b || any (`elem` b) a

listContains :: (Eq a) => ([a], [a]) -> Bool
listContains (a, b) = all (`elem` a) b || all (`elem` b) a

-- Takes a string in the form "a-b,c-d" and returns a tuple of the ranges ([a..b], [c..d])
stringToFullSections :: String -> ([Int], [Int])
stringToFullSections rs = (head rangesList, last rangesList)
    where rangesList = map stringToListRange $ splitOn "," rs

stringToListRange :: String -> [Int]
stringToListRange r = [(head rangeHeadAndTail)..(last rangeHeadAndTail)]
    where rangeHeadAndTail = map read $ splitOn "-" r