-- Part 1
type Speed    = Integer
type Time     = Integer
type Distance = Integer
type Reindeer = (Speed, Time, Time)

distance :: Time -> Reindeer -> Distance
distance time (speed, stamina, rest) = timeFlying * speed
  where
    timeFlying = (div time (stamina + rest)) * stamina +
        min stamina (mod time (stamina + rest))

winningDistance :: Time -> [Reindeer] -> Distance
winningDistance time = maximum . map (distance time)
