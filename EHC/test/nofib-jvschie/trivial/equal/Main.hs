

boolbit :: Bool -> Int
boolbit False = 99
boolbit True  = 37


lex                    :: String -> [(String,String)]
lex ""                  = [("","")]
lex (c:s) | isSpace c   = lex (dropWhile isSpace s)
          | c=='A'      = [('\'':ch++"'", t) | (ch,t0:t)  <- lexLitChar s,
                                               -- head ch /= '\'' || not (null (tail ch))
                                               ch /= "'"   && t0=='B'
                          ]
lex _                   = [("","")]

lexLitChar          :: String -> [(String,String)]
lexLitChar ""       =  []
lexLitChar (c:s)
 | c /= '\\'        =  [([c],s)]
 | True             =  [([c],s)]



half :: Rational
half = 1 :% 2

third :: Rational
third = 1 :% 3




vergelijkLijst :: [[Int]] -> Bool
vergelijkLijst [] =  False
vergelijkLijst x  =  x ==[[33,34]]

main = <PRINT_INT> (boolbit ( 12+1==15 || 34/=34 || 'A'/='A' || 'C'=='D' || [[31,32]]==[[31,32]] || vergelijkLijst [[35,36]] || null(lex "") ||  half+half==fromInt 1 ))



