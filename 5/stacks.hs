import System.IO ()
import Data.Char (isSpace)

main = do
    stackString <- readFile "stacks.txt"
    let (supply, moves) = parseInput stackString
    print supply
    print $ head moves
    print $ readMessage $ runMoves supply moves

readMessage :: Supply -> [Crate]
readMessage supply = map (head . stack) supply

runMoves :: Supply -> [Move] -> Supply
runMoves supply moves = foldl applyMoveToSupply supply moves

applyMoveToSupply :: Supply -> Move -> Supply
applyMoveToSupply supply (Move count source destination) = foldl (pushCrateToStackAtIndex destination) modifiedSupply $ poppedCrates
    where (poppedCrates, modifiedSupply) = takeCratesFromSupplyAtIndex count source supply
       
pushCrateToStackAtIndex :: Int -> Supply -> Crate -> Supply
pushCrateToStackAtIndex i stacks newCrate = map (\x -> if (index x) == i then pushCrateToStack newCrate x else x) stacks
          
takeCratesFromSupplyAtIndex :: Int -> Int -> Supply -> ([Crate], Supply)
takeCratesFromSupplyAtIndex count i supply = (crates, map (\x -> if (index x) == i then poppedStack else x) supply)
    where (crates, poppedStack) = takeCratesFromStack count (supply !! (i - 1))

pushCrateToStack :: Crate -> IndexedStack -> IndexedStack
pushCrateToStack Empty stack = stack
pushCrateToStack newCrate (IndexedStack i stack) = IndexedStack i (newCrate:stack)
 
takeCratesFromStack :: Int -> IndexedStack -> ([Crate], IndexedStack)
takeCratesFromStack count (IndexedStack i stack) = (take count stack, IndexedStack i $ drop count stack)

-------------------------------------------------------------------------------------------------

data Move = Move {count :: Int, sourceIndex :: Int, destinationIndex :: Int} deriving (Show)

data Crate = Crate Char | Empty deriving (Show, Eq, Ord)
type Stack = [Crate]
data IndexedStack = IndexedStack { index :: Int, stack :: Stack} deriving (Show)
type Supply = [IndexedStack]

parseInput :: String -> (Supply, [Move])
parseInput input = (stringsToSupply supplyStrings, map parseMove $ filter (any $ not . isSpace) instructions)
    where (supplyStrings, instructions) = span (any $ not . isSpace) $ lines input

parseMove :: String -> Move
parseMove input = Move (read $ splitInput !! 1) (read $ splitInput !! 3) (read $ splitInput !! 5)
    where splitInput = words input

stringsToSupply :: [String] -> Supply
stringsToSupply (list:indexes:[]) = zipWith (IndexedStack) (map (read) $ words indexes) (map (:[]) $ stringToCrateList list)
stringsToSupply (list:rest) = zipWith (pushCrateToStack) (stringToCrateList list) (stringsToSupply rest)

stringToCrateList :: String -> [Crate]
stringToCrateList [] = []
stringToCrateList input = [(stringToCrate h)] ++ stringToCrateList t
    where (h, t) = splitAt 4 (input)

-- string of format "[c]", remove the brackets
stringToCrate :: String -> Crate
stringToCrate s
    | all isSpace s = Empty
    | otherwise = Crate (s !! 1)