import System.IO ()

main = do
    let input = getInput
    
    let oneStep = inspectAllItems input 0
    let allrounds = foldl (\x i -> runOneRound x) input [1..20]
    print allrounds

data Monkey = Monkey { identifier :: Int, items :: [Int], op :: (Int -> Int), test :: (Int -> Int), inspectCount :: Int}
instance Show Monkey where
    show (Monkey id items op test inspectCount) = "Monkey " ++ show id ++ ":\nItems: " ++ show items ++ "\nInspect count: " ++ show inspectCount ++ "\n"

runOneRound :: [Monkey] -> [Monkey]
runOneRound party = foldl (inspectAllItems) party [0..(length party - 1)]

inspectAllItems :: [Monkey] -> Int -> [Monkey]
inspectAllItems party monkeyId = case party !! monkeyId of (Monkey _ [] _ _ _) -> party 
                                                           otherwise -> inspectAllItems (inspectNextItem monkeyId party) monkeyId

inspectNextItem :: Int -> [Monkey] -> [Monkey]
inspectNextItem monkeyId party = p2
    where 
        p2 = throwItemToMonkey reducedLevel nextMonkeyId p1
        p1 = takeItemFromMonkey monkey party
        nextMonkeyId = test reducedLevel
        reducedLevel = newValue `div` 3
        newValue = op itemToInspect
        itemToInspect = head $ items
        (Monkey id items op test inspectCount) = monkey
        monkey = party !! monkeyId
        

takeItemFromMonkey :: Monkey -> [Monkey] -> [Monkey]
takeItemFromMonkey (Monkey id items op test inspectCount) party = updateMonkey (Monkey id (tail items) op test (inspectCount + 1)) party
        
throwItemToMonkey :: Int -> Int -> [Monkey] -> [Monkey]
throwItemToMonkey item monkeyId party = updateMonkey (Monkey id (items ++ [item]) op test inspectCount) party
    where (Monkey id items op test inspectCount) = party !! monkeyId
    
updateMonkey :: Monkey -> [Monkey] -> [Monkey]
updateMonkey monkey party = take ((identifier monkey)) party ++ [monkey] ++ drop (identifier monkey + 1) party

getInput :: [Monkey]
getInput = [
    (Monkey 0 [79, 98] (*19) (\i -> if (i `mod` 23) == 0 then 2 else 3) 0),
    (Monkey 1 [54, 65, 75, 74] (+6) (\i -> if (i `mod` 19) == 0 then 2 else 0) 0),
    (Monkey 2 [79, 60, 97] (^2) (\i -> if (i `mod` 13) == 0 then 1 else 3) 0),
    (Monkey 3 [74] (+3) (\i -> if (i `mod` 17) == 0 then 0 else 1) 0)
    ]