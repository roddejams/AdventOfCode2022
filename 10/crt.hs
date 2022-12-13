import Data.List (isPrefixOf)
import System.IO ()

main = do
    programInput <- readFile "program.txt"
    let program = parseProgram programInput
    let output = runProgram program id 1
    print $ take 20 output

runProgram' :: [[Int -> Int]] -> Int -> [Int]
runProgram' program x = map fst $ scanl (\(acc, f) instr -> ((head instr . f) x, (last instr))) (x, id) (program ++ [[id], [id], [id]])

runProgram :: [[Int -> Int]] -> (Int -> Int) -> Int -> [Int]
runProgram [] f x = [f x]
runProgram ([]:program) f x = [f x] ++ (runProgram program id $ f x)
runProgram (instr:program) f x = [nextValue] ++ (runProgram program (last instr) $ nextValue)
    where nextValue = (head instr . f) x

parseProgram :: String -> [[Int -> Int]]
parseProgram = map parseInstruction . lines

parseInstruction :: String -> [Int -> Int]
parseInstruction inst 
    | isPrefixOf "addx" inst = [id, (+ (read $ drop 4 inst))]
    | otherwise = [id]