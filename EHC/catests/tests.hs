import Debug.Trace

fac 0 = 1
fac n = n * fac (n-1)

test1 :: IO ()
test1 = traceShow "foo" $ do
    traceShow "bar" test1

test2 :: IO ()
test2 = traceShow "foo" $ do
    traceShow "fooBar" (print $ fac 2000)
    traceShow "bar" test2