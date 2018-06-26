import Data.List
import Data.Bits
import Data.List.Split (chunksOf)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

raw = "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62"
input = map fromEnum raw ++ [17, 31, 73, 47, 23]
initialList = [0,1..255]
listLength = length initialList

tieAKnot :: [Int] -> Int -> Int -> Int -> Int -> [Int]
tieAKnot list position inpPosition stepSize n
 | n == 64 = list ++ [position] ++ [stepSize]
 | inpPosition == length input = tieAKnot list position 0 stepSize (n+1)
 | otherwise = tieAKnot reversed newPosition (inpPosition + 1) (stepSize + 1) n
  where inpLength = input !! inpPosition
        reversed = if (position + inpLength) >= listLength
                    then reverseCircle list position inpLength
                    else reverseStraight list position inpLength
        newPosition = if (mod (inpLength + stepSize) listLength) + position < listLength
                        then position + (mod (inpLength + stepSize) listLength)
                        else (mod (inpLength + stepSize) listLength) - (listLength - position)

reverseCircle :: [Int] -> Int -> Int -> [Int]
reverseCircle list position inpLength = drop (length endList) reversed ++ middleList ++ take (length endList) reversed
  where endList = drop position list
        beginList = take (inpLength - (length endList)) list
        middleList = fst . splitAt (listLength - (length beginList) - (length endList)) . snd . splitAt (length beginList) $ list
        reversed = reverse (endList ++ beginList)

reverseStraight :: [Int] -> Int -> Int -> [Int]
reverseStraight list position inpLength = beginList ++ (reverse middleList) ++ endList
  where beginList = take position list
        endList = drop (position + inpLength) list
        middleList = fst . splitAt (listLength - (length endList) - (length beginList)) . snd . splitAt position $ list

finalList = chunksOf 16 $ tieAKnot initialList 0 0 0 0

xOR :: (Num a, Bits a) => [a] -> a
xOR = foldl xor 0

addZero :: [String] -> [String]
addZero [] = []
addZero (x:xs) = check:addZero xs
  where check = if length x == 1 then "0" ++ x else x

to16elems = [xOR (finalList !! x) | x <- [0,1..15]]
result = concat . addZero $ [showIntAtBase 16 intToDigit x [] | x <- to16elems]
