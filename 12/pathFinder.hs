import System.IO ()
import Data.Char (ord)
import Data.List (minimumBy)
import qualified Data.Map as Map

main = do
    input <- readFile "map.txt"
    let grid = parseInput input
    --let startPoint = getStart grid
    
    let destination = getDestination grid
    let updatedGrid = Map.adjust (visitStartPoint) (x destination, y destination) grid
    let foo = visitStartPoint destination
    
    let resultFromDestination = run updatedGrid foo
    
    print resultFromDestination
    let possibleStarts = getPossibleStartPoints resultFromDestination
    
    print $ minimum $ filter (\p -> distance p > 0) possibleStarts

getPossibleStartPoints :: Grid -> [Point]
getPossibleStartPoints grid = filter (\p -> isStart p || height p == 'a') $ Map.elems grid

run :: Grid -> Point -> Grid
run grid startPoint
    | finished grid = grid
    | otherwise = run visitedGrid nextPoint
        where 
            nextPoint = getNextPoint visitedGrid
            visitedGrid = foldl (checkVertex $ distance $ visitedStartPoint) updatedGrid $ getAvailableVertexes updatedGrid visitedStartPoint
            visitedStartPoint = visitPoint startPoint
            updatedGrid = Map.adjust (visitPoint) (x startPoint, y startPoint) grid

getNextPoint :: Grid -> Point
getNextPoint grid = minimum $ filter (not . visited) $ Map.elems grid

updatePointDistance :: Int -> Point -> Point
updatePointDistance sourceDistance (Point x y h existingDistance v)
    | sourceDistance + 1 < existingDistance = (Point x y h (sourceDistance + 1) v)
    | otherwise = (Point x y h existingDistance v)

visitPoint :: Point -> Point
visitPoint (Point x y h d v) = (Point x y h d True)

visitStartPoint :: Point -> Point
visitStartPoint (Point x y h d v) = (Point x y h 0 True)

checkVertex :: Int -> Grid -> Maybe Point -> Grid
checkVertex _ grid Nothing = grid 
checkVertex sourceDistance grid (Just (Point x y h _ v)) = Map.adjust (updatePointDistance sourceDistance) (x, y) grid

getAvailableVertexes :: Grid -> Point -> [Maybe Point]
getAvailableVertexes grid source = filter (\d -> canMove d (Just source)) ([pointAt (x+1) y grid, pointAt x (y+1) grid, pointAt (x-1) y grid, pointAt x (y-1) grid])
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

canMove :: Maybe Point -> Maybe Point -> Bool
canMove _ Nothing = False
canMove Nothing _ = False
canMove (Just source) (Just destination)
    | visited source = False
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
parseLine grid line yIndex = foldl (\m (x,c) -> Map.insert (x, yIndex) (Point x yIndex c maxBound False) m) grid $ zip ([1..]) line

removeMaybe :: Maybe a -> a
removeMaybe Nothing = error "sad"
removeMaybe (Just a) = a