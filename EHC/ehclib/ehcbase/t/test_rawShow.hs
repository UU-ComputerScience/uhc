data N = S N | Z


main = let o = primRawShow "first"
	   t0 = "this should be 0 -> " ++ show (rawShow $ thunkIsEvaluated o)
	   t1 = "this should be 1 -> " ++ show (rawShow $ thunkIsEvaluated o)
           a  = S a
           b  = (a,a,a)
           c  = (b,b,b,b)
       in return (S Z) >> putStrLn t0 >> putStrLn o >> putStrLn t1
       		>> (letstrict h = primRawShow (Z,Z,Z)
		    in letstrict i = primRawShow (S Z)
		       in letstrict j = primRawShow Z

		          in return())


