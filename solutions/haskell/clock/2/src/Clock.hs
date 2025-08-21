module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {hh :: Int, mm :: Int}
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock (mod (h + div m 60) 24) (mod m 60)

toString :: Clock -> String
toString c = ad0 (hh c) ++ ":" ++ ad0 (mm c)
  where
    ad0 n = if n < 10 then "0" ++ show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm c = fromHourMin (hh c + dh) (mm c + dm)
