-- Part 1

-- first process the input to be of the form [..(operation,(x,y),(x',y'))..]

import Data.Array
import Data.List

data Light       = Off | On deriving (Eq, Show, Read)
type LightGrid   = Array (Int, Int) Light
type Instruction = (Light -> Light, (Int, Int), (Int, Int))

turnOn :: Light -> Light
turnOn _ = On

turnOff :: Light -> Light
turnOff _ = Off

toggle :: Light -> Light
toggle state
    | state == On  = Off
    | state == Off = On

initialGrid :: LightGrid
initialGrid = listArray ((0, 0), (999, 999)) $ repeat Off

update :: Ix i => (e -> e) -> Array i e -> [i] -> Array i e
update f a indices = accum (flip ($)) a (zip indices (repeat f))

executeInstruction :: LightGrid -> Instruction -> LightGrid
executeInstruction grid (operation, bottomLeft, topRight) =
    update operation grid (range (bottomLeft,topRight))

howManyLights :: [Instruction] -> Int
howManyLights = length . filter (== On) . elems . foldl' executeInstruction initialGrid
