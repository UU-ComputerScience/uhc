main = let p = q + r
           q = r + 3 + head x
           r = length s
           s = [p,q,r]
	   x = r : x
       in putStrLn (show s ++ show x)



