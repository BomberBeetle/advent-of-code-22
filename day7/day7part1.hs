import System.IO
import Data.List
import Data.Maybe
import Data.List.Split

data File =
    File {
        fname :: String,
        size :: Integer
    } deriving (Show)

data Dir =
    Dir {name :: String,
    dirs :: [Dir],
    files :: [File],
    active :: Bool
    } deriving (Show)

dirNameEquals str _dir = str == name _dir
fileNameEquals str _file = str == fname _file

processCommands cmds tree = foldl commandString tree cmds

cd tree str
    | active tree = Dir (name tree) (appendAndActivate str (dirs tree)) (files tree) False
    | otherwise = Dir (name tree) [cd x str | x <- dirs tree ] (files tree) False

appendAndActivate str _dirs
    | isNothing (find (dirNameEquals str) _dirs) = Dir str [] [] True:_dirs
    | otherwise = [if name d == str then Dir (name d) (dirs d) (files d) True else d | d <- _dirs]

appendDirToActive :: Dir -> String -> Dir
appendDirToActive tree str
    | active tree = if null (find (dirNameEquals str) (dirs tree)) then Dir (name tree) (Dir str [] [] False:dirs tree) (files tree) (active tree) else tree
    | not (active tree) && null (dirs tree) = tree
    | otherwise = Dir (name tree) [appendDirToActive x str | x <- dirs tree] (files tree) (active tree)

appendFileToActive :: Dir -> File -> Dir
appendFileToActive tree file
    | active tree = Dir (name tree) (dirs tree) ((File (fname file) (size file)):(files tree)) (active tree)
    | otherwise = Dir (name tree) [appendFileToActive x file | x <- dirs tree] (files tree) (active tree)

propagateDeactivation _dirs = [Dir (name x) (propagateDeactivation(dirs x)) (files x) False | x <- _dirs]

resetTreeToRoot tree = Dir (name tree) (propagateDeactivation(dirs tree)) (files tree) True

goBackOne tree 
    | not (null (find active (dirs tree))) = Dir (name tree) [ Dir (name x) (dirs x) (files x) False | x <- dirs tree] (files tree) True
    | active tree = tree
    | otherwise = Dir (name tree) [goBackOne x | x <- dirs tree] (files tree) False

commandString tree "$ ls" = tree
commandString tree "$ cd .." = goBackOne tree
commandString tree "$ cd /" = resetTreeToRoot tree
commandString tree ('$':' ':'c':'d':' ':rest) = cd tree rest
commandString tree ('d':'i':'r':' ':rest) = tree
commandString tree x = appendFileToActive tree (File  (splitOn " " x !! 1) (read (splitOn " " x !! 0) :: Integer))

makeFullTree string = processCommands string (Dir "/" [] [] True)

getDirVals tree = (sum [size x | x <- files tree] + sum res):res
    where res = concat [getDirVals d | d <- dirs tree]

getDirSum str = sum [x | x <- getDirVals (makeFullTree str), x <= 100000] 


main = do
    stuff <- readFile "./expect33.txt"
    print (show (makeFullTree (lines stuff)))
    print (show (getDirVals(makeFullTree (lines stuff))))
    print (show (getDirSum (lines stuff)))