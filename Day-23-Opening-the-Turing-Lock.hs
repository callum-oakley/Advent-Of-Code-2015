-- Part 1
import Data.Array

type Address = Integer
data Register = A | B deriving (Eq, Read, Show)
data Instruction = Hlf Register | Tpl Register | Inc Register | Jmp Integer |
    Jie Register Integer | Jio Register Integer deriving (Eq, Read, Show)

execute :: Instruction -> (Integer, Integer, Address) ->
    (Integer, Integer, Address)
execute (Hlf A)   (a, b, i) = (div a 2,       b, succ i)
execute (Hlf B)   (a, b, i) = (      a, div b 2, succ i)
execute (Tpl A)   (a, b, i) = (  a * 3,       b, succ i)
execute (Tpl B)   (a, b, i) = (      a,   b * 3, succ i)
execute (Inc A)   (a, b, i) = ( succ a,       b, succ i)
execute (Inc B)   (a, b, i) = (      a,  succ b, succ i)
execute (Jmp j)   (a, b, i) = (      a,       b,  i + j)
execute (Jie r j) (a, b, i)
    | r == A && even a || r == B && even b = (a, b, i + j)
    | otherwise                            = (a, b, succ i)
execute (Jio r j) (a, b, i)
    | r == A && a == 1 || r == B && b == 1 = (a, b, i + j)
    | otherwise                            = (a, b, succ i)


run :: Array Integer Instruction -> (Integer, Integer, Address) ->
    (Integer, Integer, Address)
run instructions (a, b, i)
    | i < bottom || i > top = (a, b, i)
    | otherwise = run instructions $ execute (instructions ! i) (a, b, i)
  where
    (bottom, top) = bounds instructions
