module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {hh :: Int, mm :: Int}
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock (mod (h + div m 60) 24) (mod m 60)

toString :: Clock -> String
toString c = show (div (hh c) 10) ++ show (mod (hh c) 10) ++ ":" ++ show (div (mm c) 10) ++ show (mod (mm c) 10)

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm c = fromHourMin (hh c + dh) (mm c + dm)
