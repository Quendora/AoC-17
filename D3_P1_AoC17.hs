input = 265149

data Direction = Right_ | Up_ | Left_ | Down_ deriving (Eq)

findCoord :: Int -> (Int,Int) -> Int -> Direction -> Int
findCoord tempN (coordX, coordY) step direction
  | direction == Right_ = if tempN >= input then (abs coordX) + (abs (coordY + (tempN - input))) else findCoord (tempN + step) (coordX + step, coordY) step Up_
  | direction == Up_ = if tempN >= input then (abs (coordX - (tempN - input))) + (abs coordY) else findCoord (tempN + step) (coordX, coordY + step) (step + 1) Left_
  | direction == Left_ = if tempN >= input then (abs coordX) + (abs (coordY - (tempN - input))) else findCoord (tempN + step) (coordX - step, coordY) step Down_
  | direction == Down_ = if tempN >= input then (abs (coordX + (tempN - input))) + (abs coordY) else findCoord (tempN + step) (coordX, coordY - step) (step + 1) Right_

result = findCoord 1 (0,0) 1 Right_
