module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
  case (rem year 4 == 0, rem year 100 == 0, rem year 400 == 0) of
    (True, False, _) -> True
    (True, True, True) -> True
    _ -> False