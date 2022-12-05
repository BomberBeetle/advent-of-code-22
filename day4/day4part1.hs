import System.IO
import Data.List
import Data.List.Split

fullyContained (a, b) (c, d) =  ((a <= c) && (b >= d)) || ((c <= a) && (d >= b))

tuplifyStrRanges [[a, b], [c, d]] = ((read a :: Int, read b :: Int), (read c :: Int, read d :: Int))

parseToIntTuples :: [Char] -> ((Int, Int), (Int, Int))
parseToIntTuples string = tuplifyStrRanges [splitWhen (=='-') y | y <- [x | x <- splitWhen (==',') string]]


totalFullyContained string = length [x | x <- (lines string), uncurry(fullyContained) (parseToIntTuples x)]

main = do
    stuff <- readFile "./input.txt"
    putStrLn $ (show (totalFullyContained stuff)) 