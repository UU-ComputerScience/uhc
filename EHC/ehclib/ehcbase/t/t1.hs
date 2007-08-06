-- just a module implicitly importing Prelude

module Main where

{-
showLitChar2               :: Char -> ShowS
showLitChar2 c | c > '\DEL' = showChar '\\' .
                             protectEsc isDigit (shows (fromEnum c))
showLitChar2 '\DEL'         = showString "\\DEL"
showLitChar2 '\\'           = showString "\\\\"
showLitChar2 c | c >= ' '   = showChar c
showLitChar2 '\a'           = showString "\\a"
showLitChar2 '\b'           = showString "\\b"
showLitChar2 '\f'           = showString "\\f"
showLitChar2 '\n'           = showString "\\n"
showLitChar2 '\r'           = showString "\\r"
showLitChar2 '\t'           = showString "\\t"
showLitChar2 '\v'           = showString "\\v"
showLitChar2 '\SO'          = protectEsc ('H'==) (showString "\\SO")
showLitChar2 c              = showString ('\\' : snd (asciiTab!!fromEnum c))

protectEsc p f             = f . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s

asciiTab = zip ['\NUL'..' ']
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]
-}

{-
main
  = do putStrLn
         $ concat [ "Hello World!\n"
                  , ['a','\n']
                  -- , show stderr
                  -- , fst (splitAt 5 "splitat")
                  , show (999::Int)
                  -- , show [5,6::Int]
                  {- , concat (map show [2,3::Int])
                  , showLitChar2 '\a' ""
                  , show '\a'
                  , show (snd ([(0::Int,'n'),(2,'m'),(4,'p')] !! (1::Int)))
                  , -} {- showChar (snd ('o','m')) ""
                  , showChar '\'' $ showLitChar2 '\a' $ showChar '\'' $ ""
                  , show "a\LF"
                  -- , show "a\NAK"
                  , show (2 ^ (6::Integer) :: Int)
                  -}
                  -- , show ('\a' == '\a')
                  ]
       hFlush  stdout
-}

main = putStrLn "aa"

-- main = putStrLn (showChar (fst ('o','m')) "")
-- main = putStrLn (show (snd ('o',999::Int)))

{-
f :: [forall a . Eq a => a -> a -> Bool] -> Bool
f fs = and $ map (\f -> f (3::Int) (4::Int)) fs
-}