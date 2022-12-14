import System.IO ()
import Data.List (sort)

main = do
    let input = getInput
    
    let oneStep = inspectAllItems input 0
    let allrounds = foldl (\x i -> runOneRound x) input [1..10000]
    print allrounds
    print $ calculateResult allrounds

data Monkey = Monkey { identifier :: Int, items :: [Integer], op :: (Integer -> Integer), test :: (Integer -> Int), inspectCount :: Int}
instance Show Monkey where
    show (Monkey id items op test inspectCount) = "Monkey " ++ show id ++ ":\nItems: " ++ show items ++ "\nInspect count: " ++ show inspectCount ++ "\n"
instance Eq Monkey where
    ( Monkey id1 _ _ _ _) == (Monkey id2 _ _ _ _) = id1 == id2
instance Ord Monkey where
    ( Monkey _ _ _ _ count1) <= (Monkey _ _ _ _ count2) = count1 <= count2

calculateResult :: [Monkey] -> Int
calculateResult party = result fst snd
    where (fst:snd:rest) = take 2 $ reverse $ sort party

result :: Monkey -> Monkey -> Int
result x y = (inspectCount x) * (inspectCount y)

runOneRound :: [Monkey] -> [Monkey]
runOneRound party = foldl (inspectAllItems) party [0..(length party - 1)]

inspectAllItems :: [Monkey] -> Int -> [Monkey]
inspectAllItems party monkeyId = case party !! monkeyId of (Monkey _ [] _ _ _) -> party 
                                                           otherwise -> inspectAllItems (inspectNextItem monkeyId party) monkeyId

inspectNextItem :: Int -> [Monkey] -> [Monkey]
inspectNextItem monkeyId party = p2
    where 
        p2 = throwItemToMonkey newValue nextMonkeyId p1
        p1 = takeItemFromMonkey monkey party
        nextMonkeyId = test newValue
        --reducedLevel = newValue `div` 3
        newValue = ((`mod` 9699690) . op) itemToInspect
        itemToInspect = head $ items
        (Monkey id items op test inspectCount) = monkey
        monkey = party !! monkeyId
        

takeItemFromMonkey :: Monkey -> [Monkey] -> [Monkey]
takeItemFromMonkey (Monkey id items op test inspectCount) party = updateMonkey (Monkey id (tail items) op test (inspectCount + 1)) party
        
throwItemToMonkey :: Integer -> Int -> [Monkey] -> [Monkey]
throwItemToMonkey item monkeyId party = updateMonkey (Monkey id (items ++ [item]) op test inspectCount) party
    where (Monkey id items op test inspectCount) = party !! monkeyId
    
updateMonkey :: Monkey -> [Monkey] -> [Monkey]
updateMonkey monkey party = take ((identifier monkey)) party ++ [monkey] ++ drop (identifier monkey + 1) party

getInput :: [Monkey]
getInput = [
    (Monkey 0 [54, 82, 90, 88, 86, 54] (*7) (\i -> if (i `mod` 11) == 0 then 2 else 6) 0),
    (Monkey 1 [91, 65] (*13) (\i -> if (i `mod` 5) == 0 then 7 else 4) 0),
    (Monkey 2 [62, 54, 57, 92, 83, 63, 63] (+1) (\i -> if (i `mod` 7) == 0 then 1 else 7) 0),
    (Monkey 3 [67, 72, 68] (^2) (\i -> if (i `mod` 2) == 0 then 0 else 6) 0),
    (Monkey 4 [68, 89, 90, 86, 84, 57, 72, 84] (+7) (\i -> if (i `mod` 17) == 0 then 3 else 5) 0),
    (Monkey 5 [79, 83, 64, 58] (+6) (\i -> if (i `mod` 13) == 0 then 3 else 0) 0),
    (Monkey 6 [96, 72, 89, 70, 88] (+4) (\i -> if (i `mod` 3) == 0 then 1 else 2) 0),
    (Monkey 7 [79] (+8) (\i -> if (i `mod` 19) == 0 then 4 else 5) 0)
    ]

getTestInput :: [Monkey]
getTestInput = [
    (Monkey 0 [79, 98] (*19) (\i -> if (i `mod` 23) == 0 then 2 else 3) 0),
    (Monkey 1 [54, 65, 75, 74] (+6) (\i -> if (i `mod` 19) == 0 then 2 else 0) 0),
    (Monkey 2 [79, 60, 97] (^2) (\i -> if (i `mod` 13) == 0 then 1 else 3) 0),
    (Monkey 3 [74] (+3) (\i -> if (i `mod` 17) == 0 then 0 else 1) 0)
    ]
    
    
    
    --9699690, 96577