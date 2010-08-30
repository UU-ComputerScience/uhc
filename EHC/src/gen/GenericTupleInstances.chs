%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate Haskell library code for tuple support for Generic Deriving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
%%]

%%[99 module Main
%%]

%%[99 import(Data.List (intersperse), System.Environment (getArgs))
%%]

%%[99 import({%{EH}Base.Builtin} (hsnNm2GenerReprTuple))
%%]

%%[99
u, tab, newline, sp :: ShowS
u = showChar '_'
tab = showString "  "
newline = showChar '\n'
sp = showChar ' '
comma = showChar ','
vars :: [ShowS]
vars = map ((showChar 'x' .) . shows) [1..]
paren :: ShowS -> ShowS
paren x = showChar '(' . x . showChar ')'
concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

tuple :: Int -> ShowS
tuple m = showChar '(' . showString (replicate (m-1) ',') . showChar ')'

unlinesS :: [ShowS] -> ShowS
unlinesS = foldr1 (\a b -> a . newline . b)


tupDataName :: Int -> ShowS
tupDataName m = u . showChar 'D' . u . showString "Tuple" . shows m 

tupConName :: Int -> ShowS
tupConName m = u . showChar 'C' . u . showString "Tuple" . shows m 

createDataDecls :: Int -> ShowS
createDataDecls m
  =   showString "data " . tupDataName m . newline
    . showString "data " . tupConName m

dataInstance :: Int -> ShowS
dataInstance m = let n = shows m
                     l1 =   showString "instance Datatype " . tupDataName m
                          . showString " where"
                     l2 =   tab . showString "datatypeName _ = \"" 
                          . tuple m . showChar '"'
                     l3 = tab . showString "moduleName   _ = \"Prelude\""
                 in unlinesS [l1, l2, l3]

conInstance :: Int -> ShowS
conInstance m = let n = shows m
                    l1 = showString "instance Constructor " . tupConName m . showString " where"
                    l2 = tab . showString "conName _ = \"" . tuple m . showChar '"'
                    l3 = tab . showString "conIsTuple _ = Arity " . n
                in  unlinesS [l1, l2, l3]

-- x is 0 or 1
pairPat, repName, rep, repInst, funs :: Int -> Int -> ShowS
pairPat x m = tuple m . sp . 
                (concatS $ intersperse sp (take (m - x) vars))

-- repName x m = u . showString "Rep" . shows x . showString "Tuple" . shows m
repName x m = shows $ hsnNm2GenerReprTuple m x

rep x m = let n    = shows m
              v    = take (m - x) vars
              vs   = concatS $ intersperse sp v
              recs = concatS $ intersperse (showString " :*: ") $ 
                       map (showString "Rec0 " .) v
              last = showString $ if (x == 1) then " :*: Par1" else ""
              body = recs . last
          in    showString "type " . repName x m . sp . vs
              . showString " = D1 " . tupDataName m . showString " (C1 " . tupConName m 
              . showString " (S1 NoSelector (" . body . showString ")))"

repInst x m = let n = shows m
                  y = shows x
                  vs = concatS $ intersperse sp (take (m - x) vars)
              in   showString "instance Representable" . y . sp
                 . paren (pairPat x m) . showString " (" . repName x m . sp
                 . vs . showString ") where"
                 . newline . funs x m

funs x m = 
  let v    = take (m - x) vars
      recs = concatS $ intersperse (showString " :*: ") $
               map (showString "K1 " .) v
      last = if (x == 1) then showString " :*: Par1 " . (vars !! (m-x))
                          else showString ""
      eq   = showChar '='
      body = paren (showString "M1 (M1 (M1 (" . recs . last . showString ")))")
      pat  = paren (pairPat 0 m)
  in tab . concatS (intersperse sp [showString "from" . shows x, pat, eq, body])
     . newline . 
     tab . concatS (intersperse sp [showString "to"   . shows x, body, eq, pat])


genArities :: [Int]
genArities = [0..1]

gen :: Int -> ShowS
gen m
  =   concatS 
    $ intersperse (newline . newline)
    $    [ createDataDecls m, dataInstance m, conInstance m ]
      ++ concat [ [rep a m, repInst a m] | a <- genArities ]

header :: Int -> ShowS
header maxArity
  =   showString "{-# LANGUAGE NoImplicitPrelude     #-}" . newline 
    . showString "{-# LANGUAGE EmptyDataDecls        #-}" . newline 
    . showString "{-# LANGUAGE MultiParamTypeClasses #-}" . newline 
    . showString "{-# LANGUAGE TypeSynonymInstances  #-}" . newline 
    . showString "{-# LANGUAGE TypeOperators         #-}" . newline 
    . newline
    . showString "module UHC.Generics.Tuple(" . exports . showString ") where" . newline
    . showString "import UHC.Base" . newline
  where exports = concatS $ intersperse comma [ repName a m | m <- [2..maxArity], a <- genArities ]

main :: IO ()
main = do let r :: [String] -> Int
              r (n:_) = read n
              r _     = error "Integer argument missing"
              com     =   showString "\n\n"
                        . concatS (map showChar (replicate 80 '-'))
                        . showString "\n\n"
          a <- getArgs
          let maxArity = r a
          (putStr . ($ "")) $ concatS $
            intersperse com (header maxArity : [ gen m | m <- [2..maxArity]])
%%]
