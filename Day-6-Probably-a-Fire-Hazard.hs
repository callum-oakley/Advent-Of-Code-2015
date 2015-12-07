-- Part 1
import Data.Array
import Data.List

-- first process the input to be of the form [..(operation,(x,y),(x',y'))..]

type Light       = Int
type LightGrid   = Array (Int, Int) Light
type Instruction = (Light -> Light, (Int, Int), (Int, Int))

update :: Ix i => (e -> e) -> Array i e -> [i] -> Array i e
update f a indices = accum (flip ($)) a (zip indices (repeat f))

executeInstruction :: LightGrid -> Instruction -> LightGrid
executeInstruction grid (operation, bottomLeft, topRight) =
    update operation grid (range (bottomLeft,topRight))

executeInstructions :: [Instruction] -> LightGrid
executeInstructions = foldl' executeInstruction initialGrid
    where initialGrid = listArray ((0, 0), (999, 999)) $ repeat 0

totalBrightness :: [Instruction] -> Int
totalBrightness = sum . elems . executeInstructions

-- Part 2
-- we can use totalBrightness for part 1 and part 2 as in the first case each light has brightness 1 or 0 so the number of lit lights is just the total brightness. Just feed in our different inputs.
