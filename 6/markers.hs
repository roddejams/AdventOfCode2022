import System.IO ()
import Data.List (delete)

main = do
    buffer <- readFile "buffer.txt"
    print $ locationOfFirstMarker buffer
    
listUnique :: (Eq a) => [a] -> Bool
listUnique s = all (\x -> x `notElem` (delete x s)) s

listIntoLengths :: Int -> [a] -> [[a]]
listIntoLengths x (a:rest)
    | length rest < x = [(a:rest)]
    | otherwise = (take x (a:rest)) : listIntoLengths x (rest)

locationOfFirstMessage :: String -> Int
locationOfFirstMessage s = (length $ takeWhile (not . listUnique) $ listIntoLengths 14 s) + 14

locationOfFirstMarker :: String -> Int
locationOfFirstMarker s = (length $ takeWhile (not . listUnique) $ listIntoLengths 4 s) + 4