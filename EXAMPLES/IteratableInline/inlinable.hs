

flip a b c = a c b

-- inline-en creert mogelijkheden voor verwijderen PAP.
main = putStrLn (show (flip (flip (-)) (2 :: Int) 3))