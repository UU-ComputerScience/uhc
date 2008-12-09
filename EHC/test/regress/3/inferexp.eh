-- Unification and ML Type Reconstruction (1991)
-- C. Kanellakis, Harry G. Mairson, John C. Mitchell
-- Computational Logic - Essays in Honor of Alan Robinson

-- exponential behavior of unification (if type not represented by dag)

let
f = \x -> let x0 = x
              x1 = (x0,x0)
              x2 = (x1,x1)
              x3 = (x2,x2)
              x4 = (x3,x3)
              x5 = (x4,x4)
              -- x6 = (x5,x5)
              -- x7 = (x6,x6)
              -- x8 = (x7,x7)
              -- x9 = (x8,x8)
              -- x10 = (x9,x9)
           in x5

in f 3
