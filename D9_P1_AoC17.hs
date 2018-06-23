import Data.List
import System.IO

main = do
  handle <- openFile "D9_Input.txt" ReadMode
  input <- hGetContents handle
  print (findScore input 0 0 False False)
  hClose handle

findScore :: String -> Int -> Int -> Bool -> Bool -> Int
findScore [] totalScore _ _ _ = totalScore
findScore (x:xs) totalScore lastValue garbage ignoreNext
  | ignoreNext == True = findScore xs totalScore lastValue garbage False
  | x == '<' = findScore xs totalScore lastValue True ignoreNext
  | (garbage == True) && (x == '>') = findScore xs totalScore lastValue False ignoreNext
  | x == '!' = findScore xs totalScore lastValue garbage True
  | (garbage == False) && (x == '{') = findScore xs (totalScore + newValue) newValue garbage ignoreNext
  | (garbage == False) && (x == '}') = findScore xs totalScore (lastValue - 1) garbage ignoreNext
  | otherwise = findScore xs totalScore lastValue garbage ignoreNext
  where
    newValue = lastValue + 1
