import System.IO ()
import Data.Char (ord)
import Data.List (minimumBy)
import qualified Data.Map as Map

main = do
    input <- readFile "map.txt"
    let grid = parseInput input
    let startPoint = getStart grid
    let startPoints = getPossibleStartPoints grid
    
    let shortestFromAnyStart = minimum $ map (getDestination . run grid) startPoints
    
    let result = run grid startPoint
    let destination = getDestination result
    print destination
    print startPoints



getPossibleStartPoints :: Grid -> [Point]
getPossibleStartPoints grid = filter (\p -> isStart p || height p == 'a') $ Map.elems grid

run :: Grid -> Point -> Grid
run grid startPoint
    | finished grid = grid
    | otherwise = run otherGrid nextPoint
        where 
            (nextPoint, otherGrid) = getNextPoint visitedGrid
            visitedGrid = foldl (checkVertex $ distance startPoint) grid $ getAvailableVertexes grid startPoint

getNextPoint :: Grid -> (Point, Grid)
getNextPoint grid = (visitPoint visitedPoint, updatedGrid)
    where 
        updatedGrid = Map.adjust (visitPoint) (x visitedPoint, y visitedPoint) grid
        visitedPoint = minimum $ filter (not . visited) $ Map.elems grid

updatePointDistance :: Int -> Point -> Point
updatePointDistance sourceDistance (Point x y h existingDistance v)
    | sourceDistance + 1 < existingDistance = (Point x y h (sourceDistance + 1) v)
    | otherwise = (Point x y h existingDistance v)

visitPoint :: Point -> Point
visitPoint (Point x y h d v) = (Point x y h d True)

checkVertex :: Int -> Grid -> Maybe Point -> Grid
checkVertex _ grid Nothing = grid 
checkVertex sourceDistance grid (Just (Point x y h _ v)) = Map.adjust (updatePointDistance sourceDistance) (x, y) grid

getAvailableVertexes :: Grid -> Point -> [Maybe Point]
getAvailableVertexes grid source = filter (canMove source) ([pointAt (x+1) y grid, pointAt x (y+1) grid, pointAt (x-1) y grid, pointAt x (y-1) grid])
    where (Point x y _ _ _) = source

finished :: Grid -> Bool
finished grid = all visited $ Map.elems grid

getStart :: Grid -> Point
getStart grid = head $ filter isStart $ Map.elems grid

getDestination :: Grid -> Point
getDestination grid = head $ filter isDestination $ Map.elems grid

isStart :: Point -> Bool
isStart point = height point == 'S'

isDestination :: Point -> Bool
isDestination point = height point == 'E'

canMove :: Point -> Maybe Point -> Bool
canMove _ Nothing = False
canMove source (Just destination)
    | visited destination = False
    | isStart source = height destination == 'a' || height destination == 'b'
    | isDestination destination = height source == 'z' || height source == 'y'
    | (ord $ height destination) < (ord $ height source) = True -- Can go down as much as we like
    | otherwise = (ord $ height destination) - (ord $ height source) <= 1

data Point = Point {x :: Int, y :: Int, height :: Char, distance :: Int, visited :: Bool} deriving (Show, Eq)
instance Ord Point where
    (Point _ _ _ d1 _) <= (Point _ _ _ d2 _) = d1 <= d2
    
type Grid = Map.Map (Int, Int) Point

pointAt :: Int -> Int -> Grid -> Maybe Point
pointAt x y = Map.lookup (x, y)

parseInput :: String -> Grid
parseInput input = foldl (\m (x, c) -> parseLine m c x) Map.empty $ zip ([1..]) $ lines input

parseLine :: Grid -> String -> Int -> Grid
parseLine grid line yIndex = foldl (\m (x,c) -> Map.insert (x, yIndex) (Point x yIndex c (if c == 'S' then 0 else maxBound) (c == 'S')) m) grid $ zip ([1..]) line

removeMaybe :: Maybe a -> a
removeMaybe Nothing = error "sad"
removeMaybe (Just a) = a