import System.IO ()
import Data.List (isPrefixOf, find)

main = do
    fileCommands <- readFile "commands.txt"
    let fileSystem = parseFileSystem fileCommands
    print $ minimum $ map snd $ searchDirectories (> (sizeRequiredToDelete fileSystem)) fileSystem

sizeRequiredToDelete :: FileSystem -> Int
sizeRequiredToDelete s = 30000000 - unusedSpace s

unusedSpace :: FileSystem -> Int
unusedSpace s = 70000000 - directorySize s

searchDirectories :: (Int -> Bool) -> FileSystem -> [(String, Int)] 
searchDirectories search Empty = []
searchDirectories search (File _ _) = []
searchDirectories search (Directory name content)
    | search size = (name, size) : limitedSizeChildren
    | otherwise = limitedSizeChildren
        where size = directorySize (Directory name content)
              limitedSizeChildren = concat $ map (searchDirectories search) content

directorySize :: FileSystem -> Int
directorySize Empty = 0
directorySize (File _ size) = size
directorySize (Directory _ content) = sum $ map directorySize content

parseFileSystem :: String -> FileSystem
parseFileSystem s = fst $ goToHomeDirectory $ foldl runCommand (Directory "/" [], []) (lines s)

runCommand :: Zipper -> String -> Zipper
runCommand zip command
    | command == "$ ls" = zip
    | command == "$ cd /" = goToHomeDirectory zip
    | command == "$ cd .." = leaveDirectory zip
    | isPrefixOf "$ cd" command = let dirName = (drop 5 command) in enterDirectory zip dirName
    | isPrefixOf "dir" command = let newName = (drop 4 command) in (Directory name ((Directory newName []) : content), bs)
    | otherwise = let fileSize:fileName:_ = (words command) in (Directory name ((File fileName $ read fileSize) : content), bs)
    where ((Directory name content), bs) = zip
    
enterDirectory :: Zipper -> String -> Zipper
enterDirectory (Directory name content, bs) dirName = (findDirectory content dirName, (Directory name $ filter (\x -> fileName x /= dirName) content):bs)
    
leaveDirectory :: Zipper -> Zipper
leaveDirectory (f, (Directory name content):bs) = (Directory name (f:content), bs)
    
findDirectory :: [FileSystem] -> String -> FileSystem
findDirectory content dirName = case find (\x -> fileName x == dirName) content
                          of Just f -> f
                             Nothing -> Empty

goToHomeDirectory :: Zipper -> Zipper
goToHomeDirectory (f, []) = (f, [])
goToHomeDirectory z = goToHomeDirectory (leaveDirectory z)
                             
data FileSystem = Empty | File String Int | Directory String [FileSystem] deriving Show
type FileNavigation = [String]
type Breadcrumbs = [FileSystem]
type Zipper = (FileSystem, Breadcrumbs)

fileName :: FileSystem -> String
fileName (File name _) = name
fileName (Directory name _) = name