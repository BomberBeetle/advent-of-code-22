import System.IO
import Data.List

charsToHeader string chars = if length(nub(take 4 string)) == 4 then chars else charsToHeader (drop 1 string) chars+1

main = do
    stuff <- readFile "./input.txt"
    print (show (charsToHeader stuff 4))