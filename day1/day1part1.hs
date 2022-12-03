import System.IO
import Data.List
import Data.List.Split

getMaxCalories string = maximum [sum [read b::Int | b <- a] | a <- (splitWhen (=="") (lines string))]

main = do
    stuff <- readFile "./input.txt"
    putStrLn $ (show (getMaxCalories stuff))