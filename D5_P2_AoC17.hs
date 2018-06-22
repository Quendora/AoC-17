import Data.List
import Data.Char (digitToInt)

raw = words
  "0 3 0 1 -3"

input = map (read :: String -> Int) raw
outside = length input

jump :: [Int] -> Int -> Int -> Int
jump inst countStep currPos
  | (currPos >= outside || currPos < 0) = countStep
  | otherwise = jump (updateInst inst currPos) (countStep + 1) $! currPos + (inst !! currPos)

updateInst :: [Int] -> Int -> [Int]
updateInst inst currPos = concat [fstPart,offset,sndPart]
  where splited = splitAt currPos inst
        fstPart = fst splited
        sndPart = tail $ snd splited
        offset = if elemOfCurrInd >= 3 then [elemOfCurrInd - 1] else [elemOfCurrInd + 1]
        elemOfCurrInd = inst !! currPos

result = jump input 0 0
