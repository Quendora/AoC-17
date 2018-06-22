input = 265149

fstElem (a, _, _) = a
sndElem (_, a, _) = a
trdElem (_, _, a) = a

data Direction = Right_ | Up_ | Left_ | Down_ deriving (Eq)

filterCoords :: [(Int,Int,Int)] -> (Int,Int) -> [Int]
filterCoords matrix (cX, cY) = [fstElem x | x <- matrix, sndElem x == cX, trdElem x == cY]

checkIfExists :: [(Int,Int,Int)] -> (Int, Int) -> Int
checkIfExists matrix (cX, cY) = if (filterCoords matrix (cX, cY)) == [] then 0 else head (filterCoords matrix (cX, cY))

newValue :: [(Int,Int,Int)] -> (Int,Int) -> Int
newValue matrix (cX, cY) =
  let fstCol = checkIfExists matrix (cX-1, cY+1) + checkIfExists matrix (cX, cY+1) + checkIfExists matrix (cX+1, cY+1)
      sndCol = checkIfExists matrix (cX-1, cY) + checkIfExists matrix (cX+1, cY)
      trdCol = checkIfExists matrix (cX-1, cY-1) + checkIfExists matrix (cX, cY-1) + checkIfExists matrix (cX+1, cY-1)
  in fstCol + sndCol + trdCol

findCoord :: Int -> (Int,Int) -> Int -> Direction -> [(Int,Int,Int)] -> Int -> Int
findCoord tempN (coordX, coordY) step direction matrix n
  | direction == Right_ = if tempN > input then tempN else
    if n <= 1 then findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX + 1, coordY)) (coordX + 1, coordY) step Up_ (matrix ++ [(tempN, coordX, coordY)]) (step)
        else findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX + 1, coordY)) (coordX + 1, coordY) step Right_ (matrix ++ [(tempN, coordX, coordY)]) (n-1)

  | direction == Up_ = if tempN > input then tempN else
    if n <= 1 then findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX, coordY + 1)) (coordX, coordY + 1) (step+1) Left_ (matrix ++ [(tempN, coordX, coordY)]) (step+1)
        else findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX, coordY + 1)) (coordX, coordY + 1) step Up_ (matrix ++ [(tempN, coordX, coordY)]) (n-1)

  | direction == Left_ = if tempN > input then tempN else
    if n <= 1 then findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX - 1, coordY)) (coordX - 1, coordY) step Down_ (matrix ++ [(tempN, coordX, coordY)]) (step)
        else findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX - 1, coordY)) (coordX - 1, coordY) step Left_ (matrix ++ [(tempN, coordX, coordY)]) (n-1)

  | direction == Down_ = if tempN > input then tempN else
    if n <= 1 then findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX, coordY - 1)) (coordX, coordY - 1) (step+1) Right_ (matrix ++ [(tempN, coordX, coordY)]) (step+1)
        else findCoord (newValue (matrix ++ [(tempN, coordX, coordY)]) (coordX, coordY - 1)) (coordX, coordY - 1) step Down_ (matrix ++ [(tempN, coordX, coordY)]) (n-1)

result = findCoord 1 (0,0) 1 Right_ [(1,0,0)] 1
