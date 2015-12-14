-- Part 1
import Data.Array
import Data.List

type Speed    = Integer
type Time     = Integer
type Distance = Integer
type Reindeer = Integer

distance :: Time -> (Speed, Time, Time) -> Distance
distance time (speed, stamina, rest) = timeFlying * speed
  where
    timeFlying = (div time (stamina + rest)) * stamina +
        min stamina (mod time (stamina + rest))

winningReindeer :: Time -> Array Reindeer (Speed, Time, Time) -> Reindeer
winningReindeer time stats = maximumBy compareDistance $ [0..n]
  where
    n = snd . bounds $ stats
    compareDistance x y =
        compare (distance time (stats ! x)) (distance time (stats ! y))

winningDistance :: Time -> Array Reindeer (Speed, Time, Time) -> Distance
winningDistance time stats =
    distance time (stats ! (winningReindeer time stats))

-- Part 2
update :: Ix i => (e -> e) -> Array i e -> [i] -> Array i e
update f a indices = accum (flip ($)) a (zip indices (repeat f))

maxPoints :: Time -> Array Reindeer (Speed, Time, Time) -> Integer
maxPoints time stats = maximum . elems . foldl' f zeroArray $ [1..time]
  where
    zeroArray = listArray (bounds stats) (repeat 0)
    f points t = update succ points [winningReindeer t stats]
