import System.IO
import Data.List
roundScore :: [Char] -> Int
roundScore string = (matchScore (string !! 0) (string !! 2)) + (baseScore $ string!! 2)

matchScore :: Char -> Char -> Int
matchScore 'A' 'X' = 3
matchScore 'A' 'Y' = 6
matchScore 'A' 'Z' = 0

matchScore 'B' 'X' = 0
matchScore 'B' 'Y' = 3
matchScore 'B' 'Z' = 6

matchScore 'C' 'X' = 6
matchScore 'C' 'Y' = 0
matchScore 'C' 'Z' = 3

matchScore a b = -1 

baseScore :: Char -> Int
baseScore 'X' = 1
baseScore 'Y' = 2
baseScore 'Z' = 3
baseScore b = -1

totalScore :: [Char] -> Int
totalScore string_lines = sum [ roundScore x | x <- lines string_lines]

main = do
    stuff <- readFile "./input.txt"
    putStrLn $ (show (totalScore stuff)) 