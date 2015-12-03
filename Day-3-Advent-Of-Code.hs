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
listHouses = foldl' updateHouses [(0, 0)] . map arrowToMovement
  where
    updateHouses ps@(p:_) m = (move p m):ps

noOfUniqueHouses :: String -> Int
noOfUniqueHouses = length . nub . listHouses

-- Part 2
everyOther :: [a] -> [a]
everyOther []     = []
everyOther (x:xs) = x:(everyOther $ drop 1 xs)

noOfUniqueHouses' :: String -> Int
noOfUniqueHouses' instructions = length . nub $ (santaHouses ++ roboHouses)
  where
    santaHouses = listHouses . everyOther $ instructions
    roboHouses  = listHouses . everyOther . tail $ instructions
