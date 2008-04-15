
type LazyExpr a = Oracle -> (Oracle, a)

doLazy o e = letstrict o' = primOracleEnter o
	     in letstrict r = e
	        in primOracleLeave o' r

doL e1 e2 = letstrict o1 = primOracleNewEntry
            in let x = doLazy o1 e1
               in e2 x

main = letstrict h = primInitOracle
       in doL "xxxxx" (\x -> print x >>
	     	             print primDumpOracle)
			     

