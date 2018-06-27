import Data.List

raw = "0: 3\n1: 2\n2: 4\n4: 4\n6: 5\n8: 8\n10: 6\n12: 6\n14: 6\n16: 6\n18: 8\n20: 8\n22: 12\n24: 10\n26: 9\n28: 8\n30: 8\n32: 12\n34: 12\n36: 12\n38: 12\n40: 8\n42: 12\n44: 14\n46: 14\n48: 10\n50: 12\n52: 12\n54: 14\n56: 14\n58: 14\n62: 12\n64: 14\n66: 14\n68: 14\n70: 12\n74: 14\n76: 14\n78: 14\n80: 18\n82: 17\n84: 30\n88: 14"
input = lines raw

data Direction = UP | DOWN deriving (Eq,Show)

getDepth :: (Int,Int,Direction) -> Int
getDepth (dep, _, _) = dep
getRange :: (Int,Int,Direction) -> Int
getRange (_ , ran, _) = ran
getDirection :: (Int,Int,Direction) -> Direction
getDirection (_, _, dir) = dir

depth :: String -> Int
depth line = (read :: String -> Int) $ takeWhile (/=':') line
range :: String -> Int
range line = (read :: String -> Int) $ tail $ dropWhile (/=' ') line

firewall = [(depth (input !! x), range (input !! x) - 1, DOWN)| x <- [0,1..((length input) - 1)]]
startScanners = [(depth (input !! x), 0, DOWN) | x <- [0,1..((length input) - 1)]]

changePosition :: [(Int,Int,Direction)] -> [(Int,Int,Direction)]
changePosition [] = []
changePosition (x:xs)
  | getDirection x == UP = if getRange x == 0
                              then (getDepth x, (getRange x) + 1, DOWN):changePosition xs
                              else (getDepth x, (getRange x) - 1, UP):changePosition xs
  | otherwise = if getRange x == head [getRange (firewall !! n) | n <- [0,1..((length input) - 1)], getDepth x == getDepth (firewall !! n)]
                  then (getDepth x, (getRange x) - 1, UP):changePosition xs
                  else (getDepth x, (getRange x) + 1, DOWN):changePosition xs

move :: [(Int,Int,Direction)] -> Int -> Int -> Int
move [] position severity = severity
move scanners position severity = if position == depthS
                                    then if rangeS == 0
                                      then move (changePosition $ tail scanners) (position + 1) (severity + (depthS * maxRange))
                                      else  move (changePosition $ tail scanners) (position + 1) severity
                                    else move (changePosition scanners) (position + 1) severity
  where rangeS = getRange . head $ scanners
        depthS = getDepth . head $ scanners
        maxRange = head [getRange (firewall !! n) + 1 | n <- [0,1..((length input) - 1)], depthS == getDepth (firewall !! n)]

result = move startScanners 0 0
