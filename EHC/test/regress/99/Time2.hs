{- ----------------------------------------------------------------------------------------
   what    : Testing System.Time
   expected: ok
   platform: current time depends on ... current time. Printing calendar time is environment dependent.
---------------------------------------------------------------------------------------- -}

import System.Time
import System.Locale


clock1 :: ClockTime
clock1 = TOD 10000000 1000

clock2 :: ClockTime
clock2 = TOD 20000000 2000

main :: IO ()
main = do
  now <- getClockTime
  print now
  ct  <- toCalendarTime now
  print ct

