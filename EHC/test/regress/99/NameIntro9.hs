{- ----------------------------------------------------------------------------------------
   what    : name introduction of datatype should be properly ordered before its use in pattern
   expected: ok, specific bug, forgotten dependency in name analysis, only occurring with nesting
---------------------------------------------------------------------------------------- -}

module NameIntro9 where

data L a = LL a

unL x = 
	let LL a = x
	in a

-- Is ok:
-- LL xx = LL 6

main = print $ unL (LL 6)
