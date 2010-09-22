--
-- expensive list operations,
--
-- Originally provided via:
--
--  http://community.livejournal.com/ru_lambda/44716.html
--
import System.Environment

dummy1 :: [Int] -> [Int]
dummy1 []     = []
dummy1 (x:xs) = x:dummy1 (dummy1 xs)

dummy2 :: [Int] -> [Int]
dummy2 = dum []
  where
    dum w   []   = w
    dum w (x:xs) = dum (w++[x]) (dummy2 xs)

dummy3 :: [Int] -> [Int]
dummy3 = dum []
  where
    dum w   []   = reverse w
    dum w (x:xs) = dum (x:w) (dummy3 xs)

main = do
    n <- getArgs >>= return . read . head
    print $ dummy1 [1..n]
    print $ dummy2 [1..n]
    print $ dummy3 [1..n]
