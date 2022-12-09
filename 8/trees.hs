import System.IO ()
import Data.Char

main = do
    treeInput <- readFile "trees.txt"
    let trees = parseInput $ lines treeInput
    print $ sum $ map length $ map (treesVisible trees) trees
    print $ maximum $ map maximum $ map (map (treeScore trees)) trees

data Tree = Tree { height :: Int, xPosition :: Int, yPosition :: Int } deriving Show
type Forest = [[Tree]]

treeScore :: Forest -> Tree -> Int
treeScore forest tree = (treeViewingDistance tree (treesToNorth tree forest))
                        * (treeViewingDistance tree (treesToEast tree forest))
                        * (treeViewingDistance tree (treesToWest tree forest))
                        * (treeViewingDistance tree (treesToSouth tree forest))

treeViewingDistance :: Tree -> [Tree] -> Int
treeViewingDistance source treesInLine = length $ takeWhile' (treeNotBlocked source) treesInLine

treesVisible :: Forest -> [Tree] -> [Tree]
treesVisible forest trees = filter (treeVisible forest) trees

treeVisible :: Forest -> Tree -> Bool
treeVisible forest tree = (visibleToNorth tree forest) || (visibleToEast tree forest) || (visibleToSouth tree forest) || (visibleToWest tree forest)

visibleToWest :: Tree -> Forest -> Bool
visibleToWest tree forest = treeVisibleInLine tree $ treesToWest tree forest

visibleToEast :: Tree -> Forest -> Bool
visibleToEast tree forest = treeVisibleInLine tree $ treesToEast tree forest

visibleToNorth :: Tree -> Forest -> Bool
visibleToNorth tree forest = treeVisibleInLine tree $ treesToNorth tree forest

visibleToSouth :: Tree -> Forest -> Bool
visibleToSouth tree forest = treeVisibleInLine tree $ treesToSouth tree forest

treesToEast :: Tree -> Forest -> [Tree]
treesToEast tree forest = drop ((xPosition tree) + 1) $ (forest !! (yPosition tree))

treesToWest :: Tree -> Forest -> [Tree]
treesToWest tree forest = reverse $ take (xPosition tree) $ (forest !! (yPosition tree))

treesToNorth :: Tree -> Forest -> [Tree]
treesToNorth tree forest = reverse $ map (\x -> x !! (xPosition tree)) $ take (yPosition tree) forest

treesToSouth :: Tree -> Forest -> [Tree]
treesToSouth tree forest = map (\x -> x !! (xPosition tree)) $ drop ((yPosition tree) + 1) forest

treeVisibleInLine :: Tree -> [Tree] -> Bool
treeVisibleInLine tree trees = all (treeNotBlocked tree) trees

treeNotBlocked :: Tree -> Tree -> Bool
treeNotBlocked source destination = (height source) > (height destination)

takeWhile' :: (Tree -> Bool) -> [Tree] -> [Tree]
takeWhile' f [] = []
takeWhile' f (t:ts)
    | f t = t : takeWhile' f ts
    | otherwise = [t]

parseInput :: [String] -> Forest
parseInput input = map (\x -> parseRow (fst x) (snd x)) $ zip input [0..]

parseRow :: String -> Int -> [Tree]
parseRow input rowNum = map (\x -> parseTree (fst x) (snd x) rowNum) $ zip input [0..]

parseTree :: Char -> Int -> Int -> Tree
parseTree c x y = Tree (digitToInt c) x y