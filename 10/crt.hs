import Data.List (isPrefixOf)
import System.IO ()
import Data.List (find)

main = do
    program <-readFile "program.txt"
    let output = runProgram $ lines program
    print output
    let strengths = map (\c -> (signalStrengthAtCycle output c) * c) ([20,60,100,140,180,220]) 
    print $ sum strengths

signalStrengthAtCycle :: [(Int, Int)] -> Int -> Int
signalStrengthAtCycle output cycleNumber = signalStrength
    where signalStrength = case find (\(x, c) -> c == cycleNumber) output of (Just (x, c)) -> x
                                                                             Nothing -> signalStrengthAtCycle output (cycleNumber - 1) 


runProgram :: [String] -> [(Int, Int)]
runProgram program = scanl parseInstruction (1, 1) program

-- runProgram' :: [[Int -> Int]] -> Int -> [Int]
-- runProgram' program x = map fst $ scanl (\(acc, f) instr -> ((head instr . f) x, (last instr))) (x, id) (program ++ [[id], [id], [id]])

-- runProgram :: [[Int -> Int]] -> (Int -> Int) -> Int -> [Int]
-- runProgram [] f x = [f x]
-- runProgram ([]:program) f x = [f x] ++ (runProgram program id $ f x)
-- runProgram (instr:program) f x = [nextValue] ++ (runProgram program (last instr) $ nextValue)
--     where nextValue = (head instr . f) x

-- parseProgram :: String -> Int -> Int
-- parseProgram = map parseInstruction . lines

parseInstruction :: (Int, Int) -> String -> (Int, Int)
parseInstruction (x, cycle) inst 
    | isPrefixOf "addx" inst = (x + (read $ drop 4 inst), cycle + 2)
    | otherwise = (x, cycle + 1)