-- Part 1

-- first process the input to be of the form [..(operation,(x,y),(x',y'))..]

import Data.Array
import Data.List

type Light       = Int
type LightGrid   = Array (Int, Int) Light
type Instruction = (Light -> Light, (Int, Int), (Int, Int))

turnOn :: Light -> Light
turnOn _ = 1

turnOff :: Light -> Light
turnOff _ = 0

toggle :: Light -> Light
toggle = (1 -)

initialGrid :: LightGrid
initialGrid = listArray ((0, 0), (999, 999)) $ repeat 0

update :: Ix i => (e -> e) -> Array i e -> [i] -> Array i e
update f a indices = accum (flip ($)) a (zip indices (repeat f))

executeInstruction :: LightGrid -> Instruction -> LightGrid
executeInstruction grid (operation, bottomLeft, topRight) =
    update operation grid (range (bottomLeft,topRight))

executeInstructions :: [Instruction] -> LightGrid
executeInstructions = foldl' executeInstruction initialGrid

howManyLights :: [Instruction] -> Int
howManyLights = length . filter (== 1) . elems . executeInstructions

-- Part 2

inc1 :: Light -> Light
inc1 = (+ 1)

dec1 :: Light -> Light
dec1 brightness
    | brightness > 0 = brightness - 1
    | otherwise      = 0

inc2 :: Light -> Light
inc2 = (+ 2)

totalBrightness :: [Instruction] -> Int
totalBrightness = sum . elems . executeInstructions
