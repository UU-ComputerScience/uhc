{- ----------------------------------------------------------------------------------------
   what    : correct use of field/var when having the same name, combi with incorrect propagation of defaulting substitution
   expected: ok
---------------------------------------------------------------------------------------- -}

module DataField5 where

{-
Originally:
  Results in: uhc: panic: dtiOffsetOfFld
  Because c is used both as binding and record function.
-}

data C a = C {
	c :: a
}

f = c
	where
	c = C {
		c = 5
	}
	
main = print (c f)
