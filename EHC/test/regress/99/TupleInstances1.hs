{- ----------------------------------------------------------------------------------------
   what    : various tuple instances: Eq/Ord on 2/5-tuple
   expected: ok
---------------------------------------------------------------------------------------- -}

module TupleInstances1 where

main :: IO ()
main
  = do let v1 = 3 :: Int
           v2 = 4 :: Int
       putStrLn (show ((v1,v1) == (v1,v1)))
       putStrLn (show ((v1,v1) `compare` (v1,v1)))
       putStrLn (show ((v1,v2) == (v1,v1)))
       putStrLn (show ((v1,v2) `compare` (v1,v1)))
       putStrLn (show ((v1,v2) == (v2,v1)))
       putStrLn (show ((v1,v2) `compare` (v2,v1)))
       putStrLn (show ((v1,v1,v1,v1,v1) == (v1,v1,v1,v1,v1)))
       putStrLn (show ((v1,v1,v1,v1,v1) `compare` (v1,v1,v1,v1,v1)))
       putStrLn (show ((v1,v1,v1,v1,v2) == (v1,v1,v1,v1,v1)))
       putStrLn (show ((v1,v1,v1,v1,v2) `compare` (v1,v1,v1,v1,v1)))
       putStrLn (show ((v1,v1,v1,v1,v2) == (v1,v1,v1,v2,v1)))
       putStrLn (show ((v1,v1,v1,v1,v2) `compare` (v1,v1,v1,v2,v1)))
