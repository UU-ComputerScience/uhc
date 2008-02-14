-- !!! strongly-connected components of a graph
-- (courtesy mainly of John Launchbury)

main = <PRINT_INT> sum $ concat $ stronglyConnComp edges vertices
  where
    -- here's a test graph: Figure 6.4 from SLPJ 87
    a, b, c, d, f, g, h :: Int
    a = 1
    b = 2
    c = 3
    d = 4
    f = 5
    g = 6
    h = 7
    vertices = [a,b,c,d,f,g,h]
    edges = [(b, a),
             (c, b),
             (c, d),
             (c, h),
             (d, c),
             (f, a),
             (f, g),
             (f, h),
             (g, f),
             (h, g)]

stronglyConnComp :: [(Int, Int)] -> [Int] -> [[Int]]

stronglyConnComp es vs
  = snd (span_tree (new_range reversed_edges)
                   ([],[])
                   ( snd (dfs (new_range es) ([],[]) vs) )
        )
 where
   reversed_edges = map swap es

   swap (x,y) = (y, x)

   new_range    []       w = []
   new_range ((x,y):xys) w
       = if x==w
         then (y : (new_range xys w))
         else (new_range xys w)

   span_tree r (vs,ns) []   = (vs,ns)
   span_tree r (vs,ns) (x:xs)
       | x `elem` vs = span_tree r (vs,ns) xs
       | True = span_tree r (vs',(x:ns'):ns) xs
         where
           (vs',ns') = dfs r (x:vs,[]) (r x)

dfs :: (Int -> [Int])
       -> ([Int], [Int])
       -> [Int]
       -> ([Int], [Int])
dfs r (vs,ns)   []   = (vs,ns)
dfs r (vs,ns) (x:xs) | x `elem` vs = dfs r (vs,ns) xs
                     | True = dfs r (vs',(x:ns')++ns) xs
                                   where
                                     (vs',ns') = dfs r (x:vs,[]) (r x)
