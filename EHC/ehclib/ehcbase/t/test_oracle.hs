rep x = letstrict o = primOracleNewEntry
        in let xs = letstrict o' = primOracleEnter o
                    in letstrict r = repeat x
                       in primOracleLeave o' r
	   in x:xs


main = letstrict h = primInitOracle 
       in putStrLn (take 10 (rep 'x')) >> print primDumpOracle
