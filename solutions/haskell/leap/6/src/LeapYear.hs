module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
  case (rem year 4, rem year 100, rem year 400) of
    (0, 0, 0) -> True
    (0, 0, _) -> False
    (0, _, _) -> True
    _ -> False