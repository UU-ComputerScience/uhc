{- ----------------------------------------------------------------------------------------
   what    : Testing System.Time
   expected: ok
---------------------------------------------------------------------------------------- -}

import System.Time
import System.Locale


clock1 :: ClockTime
clock1 = TOD 10000000 1000

clock2 :: ClockTime
clock2 = TOD 20000000 2000

main :: IO ()
main = do
  let dif0   = diffClockTimes clock1 clock1
      dif1   = diffClockTimes clock2 clock1
      clock3 = addToClockTime dif1 clock1
      ndif   = normalizeTimeDiff dif1 

  print (dif0   == noTimeDiff)
  print (clock3 == clock2)
  print ndif

  putStrLn (timeDiffToString ndif)
  putStrLn (formatTimeDiff defaultTimeLocale rfc822DateFormat ndif)

  let utc = toUTCTime clock2
  print utc
  putStrLn (calendarTimeToString utc)
  putStrLn (formatCalendarTime defaultTimeLocale rfc822DateFormat utc)

