
main = let o = "oink"
	   t0 = "this should be 0 -> " ++ show (thunkIsEvaluated o)
	   t1 = "this should be 1 -> " ++ show (thunkIsEvaluated o)

       in putStrLn t0 >> putStrLn o >> putStrLn t1


