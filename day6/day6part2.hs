import System.IO
import Data.List

charsToHeader string chars = if length(nub(take 14 string)) == 14 then chars else charsToHeader (drop 1 string) chars+1

main = do
    stuff <- readFile "./input.txt"
    print (show (charsToHeader stuff 14))