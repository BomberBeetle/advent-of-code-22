import System.IO
import Data.List
import Data.Char

totalVal list_str = sum [sharedEPriority x | x <- lines list_str]

sharedEPriority sack = getPriority (head (uncurry (intersect) (splitAt ((length sack) `div` 2) sack)))

getPriority c 
    | ((c < 'A') || (c > 'z')) = -100
    | (c > 'Z') = (ord c) - 96
    | (c <= 'Z') = (ord c) - 65 + 27
    | otherwise = -200

main = do
    stuff <- readFile "./input.txt"
    putStrLn $ (show (totalVal stuff)) 