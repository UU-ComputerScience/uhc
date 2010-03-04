module RecTest2 (main) where --niet exporteren anders wordt deze niet geinlined.

-- geen case anders geen store en eval achterelkaar. 
a :: Int -> String
a i = b (i - 1)

b :: Int -> String
b i = if (i < 0) then a (i - 1) else "hallob"

main = putStrLn (a (5 :: Int))