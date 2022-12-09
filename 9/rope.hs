import System.IO ()
import Data.List (nub)

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)
type Rope = [Point]
type Path = [Point]

main = do
    moves <- readFile "moves.txt"
    let rope = replicate 10 (Point 0 0)
    let paths = runRope rope $ parseInput moves
    print paths
    print $ length $ nub $ last paths

-- returns a list of paths each node took
runRope :: Rope -> [(Point -> Point)] -> [Path]
runRope rope moves = scanl (runFollow) (runMoves (head rope) moves) (tail rope)

runFollow :: [Point] -> Point -> Path
runFollow headMoves initial = scanl follow initial headMoves

follow :: Point -> Point -> Point
follow tail head
    | shouldFollow head tail = moveTail head tail
    | otherwise = tail

moveTail :: Point -> Point -> Point
moveTail head tail
    | head == tail = tail
    | (x head) > (x tail) && (y head) == (y tail) = move 'R' tail
    | (x head) < (x tail) && (y head) == (y tail) = move 'L' tail 
    | (x head) == (x tail) && (y head) > (y tail) = move 'U' tail
    | (x head) == (x tail) && (y head) < (y tail) = move 'D' tail
    | (x head) > (x tail) && (y head) > (y tail) = move 'R' $ move 'U' tail
    | (x head) > (x tail) && (y head) < (y tail) = move 'R' $ move 'D' tail
    | (x head) < (x tail) && (y head) > (y tail) = move 'L' $ move 'U' tail
    | (x head) < (x tail) && (y head) < (y tail) = move 'L' $ move 'D' tail

shouldFollow :: Point -> Point -> Bool
shouldFollow head tail = abs (x head - x tail) > 1 || abs (y head - y tail) > 1

runMoves :: Point -> [(Point -> Point)] -> [Point]
runMoves initialPoint moves = scanl (\point move -> move point) initialPoint moves

move :: Char -> Point -> Point
move 'R' (Point x y) = Point (x + 1) y
move 'L' (Point x y) = Point (x - 1) y
move 'U' (Point x y) = Point x (y + 1)
move 'D' (Point x y) = Point x (y - 1)

parseInput :: String -> [(Point -> Point)]
parseInput input = concat $ map parseLine $ lines input

parseLine :: String -> [(Point -> Point)]
parseLine (direction:rest) = replicate (read rest) $ move direction