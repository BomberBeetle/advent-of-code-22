import System.IO
import Data.List
import Data.Char

--There's probably a more gracious way to chunk this (chunksOf wasn't importing :[ )
--But for now I guess just recursion and pattern matching will do

triChunks :: [[Char]] -> [[[Char]]]
triChunks [] = []
triChunks (x:[]) = [[x]]
triChunks (x:y:[]) = [[x,y]]
triChunks (x:y:z:[]) = [[x,y,z]]
triChunks (x:y:z:zs) = [[x,y,z]] ++ triChunks zs


totalVal list_str = sum [sharedEPriority x | x <- (triChunks (lines list_str))]

sharedEPriority sacks = getPriority (head (((sacks !! 0) `intersect` (sacks !! 1)) `intersect` (sacks !! 2)))

getPriority c 
    | ((c < 'A') || (c > 'z')) = -100
    | (c > 'Z') = (ord c) - 96
    | (c <= 'Z') = (ord c) - 65 + 27
    | otherwise = -200

main = do
    stuff <- readFile "./input.txt"
    putStrLn $ (show (totalVal stuff)) 