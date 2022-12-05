import System.IO ()
import Data.Char (isSpace)

main = do
    stackString <- readFile "stacks.txt"
    let (supply, moves) = parseInput stackString
    -- print supply
    -- print $ head moves
    print $ readMessage $ runMoves supply moves

parseInput :: String -> (Supply, [Move])
parseInput input = (stringsToSupply supplyStrings, map parseMove instructions)
    where supplyStrings = take 8 inputLines
          instructions = drop 10 inputLines
          inputLines = lines input

readMessage :: Supply -> [Crate]
readMessage supply = map (stackHead . stack) supply

data Move = Move {count :: Int, sourceIndex :: Int, destinationIndex :: Int} deriving (Show)

parseMove :: String -> Move
parseMove input = Move (read $ splitInput !! 1) (read $ splitInput !! 3) (read $ splitInput !! 5)
    where splitInput = words input

applyMoveToSupply :: Supply -> Move -> Supply
applyMoveToSupply supply (Move 0 _ _) = supply
applyMoveToSupply supply (Move count source destination) = pushCrateToStackAtIndex poppedCrate destination modifiedSupply
    where (poppedCrate, modifiedSupply) = popCrateFromSupplyAtIndex source recursedSupply
          recursedSupply = applyMoveToSupply supply (Move (count - 1) source destination) 

applyMoveToSupplyKeepOrder :: Supply -> Move -> Supply
applyMoveToSupplyKeepOrder supply (Move count source destination) = foldl (\x y -> pushCrateToStackAtIndex y destination x) modifiedSupply poppedCrates
    where (poppedCrates, modifiedSupply) = takeCratesFromSupplyAtIndex count source supply
          
takeCratesFromSupplyAtIndex :: Int -> Int -> Supply -> ([Crate], Supply)
takeCratesFromSupplyAtIndex 0 index supply = ([], supply)
takeCratesFromSupplyAtIndex count index supply = (crate:rest, finalSupply)
    where (crate, finalSupply) = popCrateFromSupplyAtIndex index modifiedSupply
          (rest, modifiedSupply) = takeCratesFromSupplyAtIndex (count - 1) index supply 

runMoves :: Supply -> [Move] -> Supply
runMoves supply moves = foldl applyMoveToSupplyKeepOrder supply moves

data Crate = Crate Char | Empty deriving (Show, Eq, Ord)
type Stack = [Crate]
data IndexedStack = IndexedStack { index :: Int, stack :: Stack} deriving (Show)
type Supply = [IndexedStack]

stringsToSupply :: [String] -> Supply
stringsToSupply (list:[]) = zipWith (IndexedStack) ([1..9]) (map (:[]) $ stringToCrateList list)
stringsToSupply (list:rest) = zipWith (pushCrateToStack) (stringToCrateList list) (stringsToSupply rest)

pushCrateToStackAtIndex :: Crate -> Int -> Supply -> Supply
pushCrateToStackAtIndex newCrate i stacks = map (\x -> if (index x) == i then pushCrateToStack newCrate x else x) stacks

pushCrateToStack :: Crate -> IndexedStack -> IndexedStack
pushCrateToStack newCrate (IndexedStack i stack) = IndexedStack i (newCrate:(stackWithoutEmpty stack))

popCrateFromSupplyAtIndex :: Int -> Supply -> (Crate, Supply)
popCrateFromSupplyAtIndex i supply = (crate, map (\x -> if (index x) == i then poppedStack else x) supply)
    where (crate, poppedStack) = popCrateFromStack (supply !! (i - 1))

popCrateFromStack :: IndexedStack -> (Crate, IndexedStack)
popCrateFromStack (IndexedStack i stack) = (stackHead stack, IndexedStack i $ stackTail stack)

stackHead :: Stack -> Crate
stackHead = head . stackWithoutEmpty

stackTail :: Stack -> Stack
stackTail = tail . stackWithoutEmpty

stackWithoutEmpty :: Stack -> Stack
stackWithoutEmpty [] = []
stackWithoutEmpty (Empty:xs) = xs
stackWithoutEmpty stack = stack

stringToCrateList :: String -> [Crate]
stringToCrateList [] = []
stringToCrateList input = [(stringToCrate h)] ++ stringToCrateList t
    where (h, t) = splitAt 4 (input)

-- string of format "[c]", remove the brackets
stringToCrate :: String -> Crate
stringToCrate s
    | all isSpace s = Empty
    | otherwise = Crate (s !! 1)