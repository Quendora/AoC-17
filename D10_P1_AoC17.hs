import Data.List

input = [189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62]
initialList = [0,1..255]
listLength = length initialList

tieAKnot :: [Int] -> Int -> Int -> [Int]
tieAKnot list position 16 = list
tieAKnot list position inpPosition = tieAKnot reversed newPosition (inpPosition + 1)
  where inpLength = input !! inpPosition
        reversed = if (position + inpLength) >= listLength
                    then reverseCircle list position inpLength
                    else reverseStraight list position inpLength
        newPosition = if (position + inpLength + inpPosition) >= listLength
                        then (inpLength + inpPosition) - (listLength - position)
                        else position + inpLength + inpPosition

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

finalList = tieAKnot initialList 0 0
result = (finalList !! 0) * (finalList !! 1)
