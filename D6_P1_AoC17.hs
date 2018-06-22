import Data.List
import Data.Maybe (fromMaybe)

input = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]
numOfMB = length input

reallocate :: [Int] -> [[Int]] -> Int -> Int
reallocate memBanks seenBefore cycleNum
  | elem  memBanks seenBefore == True = cycleNum
  | otherwise = reallocate (updateMB memBanks) (seenBefore ++ [memBanks]) (cycleNum + 1)

updateMB :: [Int] -> [Int]
updateMB memBanks =
  let content = maximum memBanks
      posOfBiggest = fromMaybe 0 $ content `elemIndex` memBanks
      tempMB = (take posOfBiggest memBanks) ++ [0] ++ (drop (posOfBiggest + 1) memBanks)
  in redistribute tempMB posOfBiggest content

redistribute :: [Int] -> Int -> Int -> [Int]
redistribute tempMB posOfBiggest content
  | content == 0 = tempMB
  | otherwise =
    let addBlock = take allPos tempMB ++ [(tempMB !! allPos) + 1] ++ drop (allPos + 1) tempMB
        allPos = if (posOfBiggest + 1 == numOfMB) then 0 else posOfBiggest + 1
    in redistribute addBlock allPos (content - 1)


result = reallocate input [] 0
