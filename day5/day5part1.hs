import System.IO
import Data.List
import Data.List.Split
import Data.Char

extractInitialStackString string = head (splitOn "\n\n" string)
extractStackCommandsStr string = last(splitOn "\n\n" string)

charToList ' ' = []
charToList char = [char]

makeIStackfromString (' ':'1':_) = [[],[],[],[],[],[],[],[],[]]
makeIStackfromString (_:a:_:_:_:b:_:_:_:c:_:_:_:d:_:_:_:e:_:_:_:f:_:_:_:g:_:_:_:h:_:_:_:i:_:_:rest) = zipWith (++) (map charToList [a,b,c,d,e,f,g,h,i]) (makeIStackfromString rest) --God has abandoned us.

performCommands :: [[Char]] -> [Char] -> [[Char]]
performCommands stacks ('m':'o':'v':'e':' ':amt:' ':'f':'r':'o':'m':' ':a:' ':'t':'o':' ':b:'\n':rest) = performCommands (modifyIndex (drop amti) ai (modifyIndex (reverse(take amti (stacks !! ai))++) bi stacks)) rest
    where amti = digitToInt amt
          ai = (digitToInt a) - 1
          bi = (digitToInt b) - 1

performCommands stacks ('m':'o':'v':'e':' ':amt1:amt2:' ':'f':'r':'o':'m':' ':a:' ':'t':'o':' ':b:'\n':rest) = performCommands (modifyIndex (drop amti) ai (modifyIndex (reverse(take amti (stacks !! ai))++) bi stacks)) rest
    where amti = read [amt1,amt2] :: Int
          ai = (digitToInt a) - 1
          bi = (digitToInt b) - 1

performCommands stacks [] = stacks

first (a, b) = a
second (a, b) = b

modifyIndex :: ([a]->[a]) -> Int -> [[a]] -> [[a]]

modifyIndex func index stacks = first (splitAt index stacks) ++ [func (stacks!!index)] ++ drop 1 (second (splitAt index stacks))

stackedString str = [ head x | x <- stacked str]
stacked str = performCommands (makeIStackfromString $ extractInitialStackString str) (extractStackCommandsStr str)


main = do
    stuff <- readFile "./input.txt"
    print (show (stackedString stuff))