{- ----------------------------------------------------------------------------------------
   what    : Floating functions, on Float
   expected: all ok
---------------------------------------------------------------------------------------- -}

module FloatingFloat1 where

main
  = do p sin (pi / 2)
       p sin (pi / 4)
       p cos (pi / 2)
       p cos (pi / 4)
       p tan (pi / 2)
       p tan (pi / 4)
       p asin 1
       p acos 1
       p atan 1
       p exp 1
       p log (exp 2)
       p sqrt 2
       p sqrt 4
       p sinh 1
       p cosh 1
       p tanh 1
  where p :: (Float -> Float) -> Float -> IO ()
        p f x = putStrLn (show (f x))
