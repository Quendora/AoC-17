import Data.List
import System.IO

main = do
  handle <- openFile "D9_Input.txt" ReadMode
  input <- hGetContents handle
  print (findScore input 0 False False)
  hClose handle

findScore :: String -> Int -> Bool -> Bool -> Int
findScore [] totalScore _ _ = totalScore
findScore (x:xs) totalScore garbage ignoreNext
  | ignoreNext == True = findScore xs totalScore garbage False
  | x == '!' = findScore xs totalScore garbage True
  | (garbage == True) && (x == '>') = findScore xs totalScore False ignoreNext
  | garbage == True = findScore xs (totalScore + 1) garbage ignoreNext
  | x == '<' = findScore xs totalScore True ignoreNext
  | otherwise = findScore xs totalScore garbage ignoreNext
