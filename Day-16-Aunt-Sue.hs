-- Part 1
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as Map

type Sue = Map.Map String Int

matchingSue :: Sue -> [Sue] -> Int
matchingSue tape = succ . fromJust . findIndex (flip Map.isSubmapOf tape)
