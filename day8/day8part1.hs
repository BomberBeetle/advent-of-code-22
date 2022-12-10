import System.IO
import Data.List
import Data.Char
import Data.Bits

allVisibleTreesFromInput str = foldr1 (zipWith(zipWith (.|.))) [(visibleL grid) , (visibleR grid) , (visibleU grid) , (visibleD grid)]
    where grid = [ map digitToInt x | x <- lines str]

sumVisibleTreesFromInput str = sum [sum x | x <- allVisibleTreesFromInput str]

visibleList :: Int -> [Int] -> [Int]
visibleList _max [] = []
visibleList _max remain
    | (head remain) > _max = 1:(visibleList (head remain) (drop 1 remain))
    | otherwise = 0:(visibleList _max (drop 1 remain))

visibleL :: [[Int]] -> [[Int]]
visibleL grid = map (visibleList (-1)) grid

visibleR :: [[Int]] -> [[Int]]
visibleR grid = map (\l -> reverse (visibleList (-1) (reverse l))) grid

visibleU :: [[Int]] -> [[Int]]
visibleU grid = transposeGrid (map (visibleList (-1)) (transposeGrid grid))

visibleD :: [[Int]] -> [[Int]]
visibleD grid = transposeGrid (map (\l -> reverse (visibleList (-1) (reverse l))) (transposeGrid grid))

transposeGrid :: [[Int]] -> [[Int]]
transposeGrid ([]:xs) = []
transposeGrid grid = [head x | x <- grid]:(transposeGrid [drop 1 x | x <- grid])

main = do
    stuff <- readFile "./input.txt"
    print (show ([ map digitToInt x | x <- lines stuff]))
    print (show (transposeGrid [ map digitToInt x | x <- lines stuff]))
    print (show (transposeGrid (transposeGrid [ map digitToInt x | x <- lines stuff])))
    print (show (allVisibleTreesFromInput stuff))
    print (show (sumVisibleTreesFromInput stuff))