{- ----------------------------------------------------------------------------------------
   what    : Name analysis, function args
   expected: error message about mismatch in nr of args
---------------------------------------------------------------------------------------- -}

module MissingPatternMatch1 where

unwrap Nothing  (Just x)               = x
unwrap (Just x) {- missing arg here -} = x

main = return ()
