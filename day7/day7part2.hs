import System.IO
import Data.List
import Data.Maybe
import Data.List.Split

deleteSize :: Integer -> Integer
deleteSize rootSize = 30000000 - (70000000 - rootSize)

data File =
    File {
        fname :: String,
        size :: Integer
    } deriving (Show)

data Dir =
    Dir {name :: String,
    dirs :: [Dir],
    files :: [File],
    active :: Bool,
    dirsize :: Integer
    } deriving (Show)

first (a, b) = a
second (a, b) = b

dirNameEquals str _dir = str == name _dir
fileNameEquals str _file = str == fname _file

processCommands cmds tree = foldl commandString tree cmds

cd tree str
    | active tree = Dir (name tree) (appendAndActivate str (dirs tree)) (files tree) False 0
    | otherwise = Dir (name tree) [cd x str | x <- dirs tree ] (files tree) False 0

appendAndActivate str _dirs
    | isNothing (find (dirNameEquals str) _dirs) = Dir str [] [] True 0:_dirs 
    | otherwise = [if name d == str then Dir (name d) (dirs d) (files d) True 0 else d | d <- _dirs]

appendDirToActive :: Dir -> String -> Dir
appendDirToActive tree str
    | active tree = if null (find (dirNameEquals str) (dirs tree)) then Dir (name tree) (Dir str [] [] False 0:dirs tree) (files tree) (active tree) 0 else tree
    | not (active tree) && null (dirs tree) = tree
    | otherwise = Dir (name tree) [appendDirToActive x str | x <- dirs tree] (files tree) (active tree) 0

appendFileToActive :: Dir -> File -> Dir
appendFileToActive tree file
    | active tree = Dir (name tree) (dirs tree) ((File (fname file) (size file)):(files tree)) (active tree) 0
    | otherwise = Dir (name tree) [appendFileToActive x file | x <- dirs tree] (files tree) (active tree) 0

propagateDeactivation _dirs = [Dir (name x) (propagateDeactivation(dirs x)) (files x) False 0 | x <- _dirs]

resetTreeToRoot tree = Dir (name tree) (propagateDeactivation(dirs tree)) (files tree) True 0

goBackOne tree 
    | not (null (find active (dirs tree))) = Dir (name tree) [ Dir (name x) (dirs x) (files x) False 0| x <- dirs tree] (files tree) True 0
    | active tree = tree
    | otherwise = Dir (name tree) [goBackOne x | x <- dirs tree] (files tree) False 0

commandString tree "$ ls" = tree
commandString tree "$ cd .." = goBackOne tree
commandString tree "$ cd /" = resetTreeToRoot tree
commandString tree ('$':' ':'c':'d':' ':rest) = cd tree rest
commandString tree ('d':'i':'r':' ':rest) = tree
commandString tree x = appendFileToActive tree (File  (splitOn " " x !! 1) (read (splitOn " " x !! 0) :: Integer))

makeFullTree string = calculateDirSizes (processCommands string (Dir "/" [] [] True 0))

calculateDirSizes tree = Dir (name tree) sized_dirs (files tree) (active tree) ((sum [dirsize d | d <- sized_dirs]) + (sum [size f | f <- files tree]))
    where sized_dirs = [calculateDirSizes d | d <- dirs tree]



flattenDirVals tree = (dirsize tree):(concat [flattenDirVals d | d <- dirs tree])

getDirSum str = foldr1 min [x | x <- dirVals, x >= (deleteSize (head(dirVals)))] 
    where dirVals = flattenDirVals (makeFullTree str)
main = do
    stuff <- readFile "./input.txt"
    print (show (makeFullTree (lines stuff)))
    print (show (flattenDirVals(makeFullTree (lines stuff))))
    print (show (getDirSum (lines stuff)))