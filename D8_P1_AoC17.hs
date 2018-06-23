raw = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
input = lines raw

instructions :: [(String,Integer)] -> [String] -> [(String,Integer)]
instructions list [] = list
instructions list (x:xs) =
  let
    line = words x
    register = line !! 0
    value = (read :: String -> Integer) (line !! 2)
    fstCheckList = if checkIfExists list register == 0 then changeList list (register,0) else list
    sndCheckList = if checkIfExists fstCheckList (line !! 4) == 0 then changeList fstCheckList ((line !! 4),0) else fstCheckList
  in if (condition sndCheckList (line !! 4) (line !! 5) ((read :: String -> Integer) (line !! 6))) == True
    then instructions (changeList sndCheckList (modify sndCheckList register (line !! 1) value)) xs
    else instructions (sndCheckList) xs

modify :: [(String,Integer)] -> String -> String -> Integer -> (String, Integer)
modify list register action value =
  if action == "inc"
    then (register, currentValue + value)
    else (register, currentValue - value)
  where currentValue = head $ regValue list register


changeList :: [(String,Integer)] -> (String,Integer) -> [(String,Integer)]
changeList list tuple =
  let
    indexOfTuple = [ x | x <- [0,1..((length list)-1)], (fst $ list !! x) == (fst tuple)]
  in if indexOfTuple == []
      then list ++ [tuple]
      else (take (head indexOfTuple) list) ++ [tuple] ++ (drop ((head indexOfTuple) + 1) list)

condition :: [(String,Integer)] -> String -> String -> Integer -> Bool
condition list register cond value
  | cond == ">" = if regVal > value then True else False
  | cond == ">=" = if regVal >= value then True else False
  | cond == "<" = if regVal < value then True else False
  | cond == "<=" = if regVal <= value then True else False
  | cond == "==" = if regVal == value then True else False
  | cond == "!=" = if regVal /= value then True else False
  where regVal = head $ regValue list register

checkIfExists :: [(String,Integer)] -> String -> Integer
checkIfExists list register =
  if  val == []
    then 0
    else head val
  where val = regValue list register

regValue :: [(String,Integer)] -> String -> [Integer]
regValue list register = [snd $ list !! x | x <- [0,1..((length list)-1)], (fst $ list !! x) == register]

finalList = instructions [] input
res = maximum [snd $ finalList !! x | x <- [0,1..((length finalList)-1)]]
