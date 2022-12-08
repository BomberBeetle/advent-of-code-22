import System.IO
import Data.List
import Data.Maybe
import Data.List.Split

data File =
    File {
        fname :: String,
        size :: Int
    } deriving (Show)

data Dir =
    Dir {name :: String,
    dirs :: [Dir],
    files :: [File],
    active :: Bool
    } deriving (Show)

dirNameEquals str _dir = str == name _dir
fileNameEquals str _file = str == fname _file

processCommands rest tree = foldl commandString tree rest

cd tree str
    | active tree = Dir (name tree) (appendAndActivate str (dirs tree)) (files tree) False
    | not (active tree) && null (dirs tree) = tree
    | otherwise = Dir (name tree) [cd x str | x <- dirs tree ] (files tree) (active tree)

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
    | active tree = if null (find (fileNameEquals (fname file)) (files tree)) then Dir (name tree) (dirs tree) ((File (fname file) (size file)):(files tree)) (active tree) else tree
    | not (active tree) && null (dirs tree) = tree
    | otherwise = Dir (name tree) [appendFileToActive x file | x <- dirs tree] (files tree) (active tree)

propagateDeactivation _dirs = [Dir (name x) (propagateDeactivation(dirs x)) (files x) False | x <- _dirs]

resetTreeToRoot tree = Dir (name tree) (propagateDeactivation(dirs tree)) (files tree) True

commandString tree "$ ls" = tree
commandString tree ('$':' ':'c':'d':' ':rest) = if rest=="/" then resetTreeToRoot tree else cd tree rest
commandString tree ('d':'i':'r':' ':rest) = appendDirToActive tree rest
commandString tree x = appendFileToActive tree (File  (splitOn " " x !! 1) (read (splitOn " " x !! 0) :: Int))

getDirVals string = processCommands string (Dir "/" [] [] True)


main = do
    stuff <- readFile "./input.txt"
    print (show (getDirVals (lines stuff)))