{-# OPTIONS_GHC -fexcess-precision #-}

import System.Environment

integrate1D :: Double -> Double -> (Double->Double) -> Double
integrate1D l u f | l `seq` u `seq` False = undefined
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum 
      [ (f l)*0.5,
        f (l+d),
        f (l+(2.0*d)),
        f (l+(3.0*d)),
        f (l+(4.0*d)),
        f (u-(3.0*d)),
        f (u-(2.0*d)),
        f (u-d),
        (f u)*0.5]

integrate2D l1 u1 l2 u2 f = integrate1D l2 u2 
				    (\y->integrate1D l1 u1 
						  (\x->f x y))

zark u v = integrate2D 0.0 u 0.0 v (\x->(\y->x*y))

ints :: [Double]
ints = [1.0..]
zarks = zipWith zark ints (map (2.0*) ints)
rtotals = head zarks : zipWith (+) (tail zarks) rtotals
rtotal n = rtotals!!n

is :: [Double]
is = map (**4) ints
itotals :: [Double]
itotals = head is : zipWith (+) (tail is) itotals
itotal :: Int -> Double
itotal n = itotals!!n

es :: [Double]
es = map (**2) (zipWith (-) rtotals itotals)
etotal :: Int -> Double
etotal n = sum (take n es)

-- The (analytical) result should be zero
main = do
	[range] <- getArgs
	putStrLn $ show $ (==0.0) $ etotal $ read range


