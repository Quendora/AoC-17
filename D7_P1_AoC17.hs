import Data.List

input = lines "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
n = length input
start =  head $ words $ head input


goDeeper :: String -> Int -> [String]
goDeeper program depth = if search == [] then [program, show depth] else goDeeper (head (search !! 0)) (depth+1)
  where
    search = [take 1 (words $ input !! x) | x <- [0,1..(n-1)], any (==True) (map (isInfixOf program) (drop 3 $ words $ input !! x))]

result = goDeeper start 0


[(read :: String -> Int) (tail $ init "(12)") | 2 == 2]
