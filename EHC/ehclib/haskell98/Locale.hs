module Locale (
    TimeLocale(..), defaultTimeLocale
  ) where

import System.Locale ( 
	-- just the bits that are specified by Haskell 98
	TimeLocale(TimeLocale,wDays,months,amPm,dateTimeFmt,
		   dateFmt,timeFmt,time12Fmt),
        defaultTimeLocale
    )
