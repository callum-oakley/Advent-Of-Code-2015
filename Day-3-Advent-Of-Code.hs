-- Part 1
import Data.List

type House = (Integer, Integer)
type Movement = (Integer, Integer)

move :: House -> Movement -> House
move (x, y) (dx, dy) = (x + dx, y + dy)

arrowToMovement :: Char -> Movement
arrowToMovement a
    | a == '>' = ( 1,  0)
    | a == '<' = (-1,  0)
    | a == '^' = ( 0,  1)
    | a == 'v' = ( 0, -1)

listHouses :: String -> [House]
listHouses = foldr updateHouses [(0, 0)] . map arrowToMovement
  where
    updateHouses m ps@(p:_) = (move p m):ps

noOfUniqueHouses :: String -> Int
noOfUniqueHouses = length . nub . listHouses
