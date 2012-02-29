{- ----------------------------------------------------------------------------------------
   what    : mfix tying the knot should work
   expected: ok
---------------------------------------------------------------------------------------- -}

module MonadFix1 where

import Data.IORef
-- import FixIO
-- import System.IO
import Control.Monad.Fix

returnIO :: a -> IO a
returnIO = return 

data Shape w = Shape { 
	  getPos :: IO ((Int,Int))
	, getX :: IO Int
	, getY :: IO Int
	, shapeTail' :: w 
}

shape x y tail self = do
	xRef <- newIORef x 
	yRef <- newIORef y 
	tail' <- tail 
	returnIO Shape { 
		  getPos = do { ; x <- getX self
	   								; y <- getY self
										; putStrLn "hi"
										; return (x,y)
	 								}
		, getX = readIORef xRef
		, getY = readIORef yRef
		, shapeTail' = tail' self
	}

main = do
	s1 <- mfix $ shape 10 20 (return (\_ -> ()))
	(x,y) <- getPos s1
	putStrLn ("x : " ++ (show x) ++ ", y : " ++ (show y))

