import System.IO
import Data.List
import Data.Char

calculateScenicScoreAt x y grid = visible size (reverse (range 0 x row)) * visible size (range (x+1) (length row) row) * visible size (reverse(range 0  y column)) * visible size (range (y+1) (length column) column)
    where row = grid !! y
          column = [ r !! x | r <- grid]
          size = (grid !! y) !! x

range start stop lst = take (stop-start) (drop start lst)

visible _ [] = 0
visible size row = if head row >= size then 1 else visible size (drop 1 row) + 1

computeScenicScores str = [ [calculateScenicScoreAt x y grid | x <- [0,1..length (grid !! y) - 1]] | y <- [0, 1..length grid - 1] ]
    where grid = [ map digitToInt x | x <- lines str]

maxScenicScores str = maximum [maximum row | row <- computeScenicScores str]

main = do
    le_input <- readFile "input.txt"
    print $ maxScenicScores le_input
