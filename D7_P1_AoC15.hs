import Data.List
import Data.Bits
import Data.Word

raw = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nd OR e -> f\ny RSHIFT 2 -> g\nNOT x -> h\nx LSHIFT 2 -> i"
input = map words $ lines raw

-- 123 -> x           3
-- NOT y -> i         4
-- x AND y -> d       5
-- x OR y -> e        5
-- x LSHIFT 2 -> f    5
-- y RSHIFT 2 -> g    5

findValue :: [[String]] -> String -> Word16
findValue list name
  | elem "NOT" lineWithValue = if any (`elem` ['0','1'..'9']) value1
                                then complement value1Int             -- DO ZMIANY
                                else complement (findValue reduceList value1)

  | elem "AND" lineWithValue = if any (`elem` ['0','1'..'9']) value0
                                then if any (`elem` ['0','1'..'9']) value2
                                      then (.&.) value0Int value2Int
                                      else (.&.) value0Int (findValue reduceList value2)
                                else if any (`elem` ['0','1'..'9']) value2
                                      then (.&.) (findValue reduceList value0) value2Int
                                      else (.&.) (findValue reduceList value0) (findValue reduceList value2)

  | elem "OR" lineWithValue = if any (`elem` ['0','1'..'9']) value0
                                then if any (`elem` ['0','1'..'9']) value2
                                      then (.|.) value0Int value2Int
                                      else (.|.) value0Int (findValue reduceList value2)
                                else if any (`elem` ['0','1'..'9']) value2
                                      then (.|.) (findValue reduceList value0) value2Int
                                      else (.|.) (findValue reduceList value0) (findValue reduceList value2)

  | elem "LSHIFT" lineWithValue = if any (`elem` ['0','1'..'9']) value0
                                    then shiftL value0Int valueShift
                                    else shiftL (findValue reduceList value0) valueShift

  | elem "RSHIFT" lineWithValue = if any (`elem` ['0','1'..'9']) value0
                                    then shiftR value0Int valueShift
                                    else shiftR (findValue reduceList value0) valueShift

  | otherwise = if any (`elem` ['0','1'..'9']) value0
                  then value0Int
                  else findValue reduceList value0

  where index = head [x | x <- [0,1..((length input) - 1)], last (input !! x) == name]
        lineWithValue = list !! index
        value0 = lineWithValue !! 0
        value1 = lineWithValue !! 1   ---  PRZEROBIC NA INTY
        value2 = lineWithValue !! 2
        value0Int = convertToWord16 0
        value1Int = convertToWord16 1
        value2Int = convertToWord16 2
        valueShift = (read :: String -> Int) (lineWithValue !! 2)
        reduceList :: [[String]]
        reduceList = (fst splited) ++ (tail $ snd splited)
          where splited = splitAt index list
        convertToWord16 n = (read :: String -> Word16) (lineWithValue !! n)


result = findValue input "x"
