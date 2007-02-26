-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Repair.Examples where

import Data.List (nub)
import Top.Types
import Top.Repair.AExpr
import Top.Repair.Transformations
import Top.Repair.Repair

-- * Repair test functions

-- | perform repair, interpret result in the form of a messagestring 
testRepair :: RepairInfo info => [Transform info] -> Int -> AExpr info -> (Maybe (AExpr info), String)
testRepair trans depth expr = 
    let result = nub (doRepair (map everywhere trans) depth expr)
    in
        case result of
        [] ->   (Nothing, "repair failed")
        resultexprs -> 
            if (resultexprs==[(expr, depth)]) 
            then (Just expr, "nothing to repair")
            else
                if ((length resultexprs) > 1)
                then 
                    let best = selectBest resultexprs 
                    in ((fst best), "more then 1 solution: " ++ " " ++ (snd best))
                else (Just (fst (head resultexprs)), "repair succeeded")

-- | select the best solution (least number of transformation steps)
selectBest :: RepairInfo info => [(AExpr info, Int)] -> (Maybe (AExpr info), String)
selectBest resultlist = 
    let bestsofar ((res, depth):ress) [bsf@(_,bsfdepth)] | depth>bsfdepth = bestsofar ress [(res,depth)]
                                                         | depth==bsfdepth = bestsofar ress []
                                                         | otherwise = bestsofar ress [bsf]
        bestsofar [] [bsf] = Just (fst bsf)
        bestsofar (r:rs) [] = bestsofar rs [r]
        bestsofar _ _ = Nothing
    in
        case (bestsofar resultlist []) of
        Nothing -> (Nothing, "no unique best solution")
        Just bsfr -> (Just bsfr, "best solution selected")

-- | output results of the repair test into a string
printRepairTestResult :: RepairInfo info => AExpr info -> Maybe (AExpr info) -> (Maybe (AExpr info), String) -> (String, Bool)
printRepairTestResult input expected (output, msg) =
    let
        success = (expected==output)
        result =    "* Repairtest result:\n" ++ 
                    "input    : " ++ (showSimple input) ++ "\n" ++
                    "astoutput: " ++ (show output) ++ "\n" ++
                    "output   : " ++ 
                    (case output of 
                        Just e -> (showSimple e)
                        Nothing -> (show output)) ++ "\n" ++ 
                    "info     : " ++ (show msg) ++ "\n" ++
                    "expected : " ++ 
                    (case expected of 
                        Just e -> (showSimple e)
                        Nothing -> (show expected)) ++ "\n" ++ 
                    if (success) then "* Test succeeded.\n\n" else "* Test failed\n\n"
    in
        (result, success)
 
-- | output results of the type test into a string
printTypeTestResult :: RepairInfo info => AExpr info -> Bool -> Bool -> (String, Bool)
printTypeTestResult input expected output = 
    let
        success = (expected==output)
        result =    "* Typetest result:\n" ++ 
                    "input    : " ++ (showSimple input) ++ "\n" ++
                    "output   : " ++ (show output) ++ "\n" ++
                    "expected : " ++ (show expected) ++ "\n" ++
                    if (success) then "* Test succeeded.\n\n" else "* Test failed\n\n"
    in (result, success)

-- shorthand
v1 :: Tp
v1 = TVar 0
v2 :: Tp
v2 = TVar 1
v3 :: Tp
v3 = TVar 2
tv1 :: (Maybe Tp)
tv1 = (Just v1)
tv2 :: (Maybe Tp)
tv2 = (Just v2)

i :: Tp
i = intType
b :: Tp
b = boolType
c :: Tp
c = charType
f :: Tp
f = floatType
l :: Tp -> Tp
l = listType
t :: Tps -> Tp
t = tupleType
it :: (Maybe Tp)
it = Just i
bt :: (Maybe Tp)
bt = Just b
ct :: (Maybe Tp)
ct = Just c
ft :: (Maybe Tp)
ft = Just f
lit :: (Maybe Tp)
lit = Just (l i)
ibt :: (Maybe Tp)
ibt = Just (i .->. b)
iit :: (Maybe Tp)
iit = Just (i .->. i)
lbt :: (Maybe Tp)
lbt = Just (l b)
ibbt :: (Maybe Tp)
ibbt = Just (i .->. b .->. b)
ib_it :: (Maybe Tp)
ib_it = Just ((i .->. b) .->. i)
iibt :: (Maybe Tp)
iibt = Just (i .->. i .->. b)
biit :: (Maybe Tp)
biit = Just (b .->. i .->. i)
lbbt :: (Maybe Tp)
lbbt = Just (l b .->. b)
iibbt :: (Maybe Tp)
iibbt = Just (i .->. i .->. b .->. b)
ilbbt :: (Maybe Tp)
ilbbt = Just (i .->. l b .->. b)
icfbt :: (Maybe Tp)
icfbt = Just (i .->. c .->. f .->. b)
headt :: (Maybe Tp)
headt = Just ((l v1) .->. v1)
filtert :: (Maybe Tp)
filtert = Just ((v2 .->. b) .->. (l v2) .->. (l v2))
event :: (Maybe Tp)
event = Just (v3 .->. b)
tibt :: (Maybe Tp)
tibt = Just (t [i, b] .->. i)
iitiibt :: (Maybe Tp)
iitiibt = Just (i .->. i .->. t [i, i] .->. b)

-- expressions
e1   :: AExpr (Maybe Tp)
e1   = App tv1 (Var ibbt "a") [(Var bt "b"), (Var it "c")]
e1'  :: AExpr (Maybe Tp)
e1'  = App tv1 (Var ibbt "a") [(Var it "c"), (Var bt "b")]
e2   :: AExpr (Maybe Tp)
e2   = App tv1 (Var iibbt "a") [(Var bt "b"), (Var it "c"), (Var it "c")]
e2'  :: AExpr (Maybe Tp)
e2'  = App tv1 (Var iibbt "a") [(Var it "c"), (Var it "c"), (Var bt "b")]
e3   :: AExpr (Maybe Tp)
e3   = App tv1 (Var iit "a") [(App tv2 (Var biit "c") [(Var  it "e"), (Var bt "d")])]
e3'  :: AExpr (Maybe Tp)
e3'  = App tv1 (Var iit "a") [(App tv2 (Var biit "c") [(Var bt "d"), (Var it "e")])]
e4   :: AExpr (Maybe Tp)
e4   = App tv1 (Var ibbt "a") [(Var bt "b"), (App tv2 (Var biit "c") [(Var it "d"), (Var bt "e")])]
e4'  :: AExpr (Maybe Tp)
e4'  = App tv1 (Var ibbt "a") [(App tv2 (Var biit "c") [(Var bt "e"), (Var it "d")]), (Var bt "b")]
e5   :: AExpr (Maybe Tp)
e5   = App tv1 (Var ilbbt "f") [(Var it "y"), (Var bt "x")]
e5'  :: AExpr (Maybe Tp)
e5'  = App tv1 (Var ilbbt "f") [(Var it "y"), (Lst lbt [(Var bt "x")])]
e6   :: AExpr (Maybe Tp)
e6   = App tv1 (Var ilbbt "f") [(Var bt "x"), (Var it "y")]
e6'  :: AExpr (Maybe Tp)
e6'  = App tv1 (Var ilbbt "f") [(Var it  "y"), (Lst lbt [(Var bt "x")])]
e7   :: AExpr (Maybe Tp)
e7   = App tv1 (Var ibbt "f") [(Var bt "x")]
e7'  :: AExpr (Maybe Tp)
e7'  = App tv1 (Var ibbt "f") [(Blk Nothing), (Var bt "x")]
e8   :: AExpr (Maybe Tp)
e8   = App tv1 (Var ibt "f") [(Var it "x"), (Var bt "y")]
e8'  :: AExpr (Maybe Tp)
e8'  = App tv1 (Var ibt "f") [(Var it "x")]
e9   :: AExpr (Maybe Tp)
e9   = App tv1 (Var ibt "f") [(Var it "x"), (Var bt "y")]
e9'  :: AExpr (Maybe Tp)
e9'  = App tv1 (Var ibt "f") [(Var it "x")]
e10  :: AExpr (Maybe Tp)
e10  = App tv1 (Var icfbt "f") [(Var bt "x"), (Var ct "y"), (Var ft "z")]
e10' :: AExpr (Maybe Tp)
e10' = App tv1 (Var icfbt "f") [(Blk Nothing), (Var ct"y"), (Var ft "z")]
e11  :: AExpr (Maybe Tp)
e11  = App tv1 (Var headt "head") [(Var filtert "filter"),(Var event "even"), (Var lit "xs")]
e11' :: AExpr (Maybe Tp)
e11' = App tv1 (Var headt "head") [(App Nothing (Var filtert "filter") [(Var event "even"), (Var lit "xs")])]
e12  :: AExpr (Maybe Tp)
e12  = App tv1 (Var icfbt "f") [(Var it "a"), (App tv2 (Var ct "b") [(Var ft "c")])]
e12' :: AExpr (Maybe Tp)
e12' = App tv1 (Var icfbt "f") [(Var it "a"), (Var ct "b"), (Var ft"c")]    
e13  :: AExpr (Maybe Tp)
e13  = App tv1 (Var tibt "f") [Tup tv2 [(Var bt "b"), (Var it "a")]]
e13' :: AExpr (Maybe Tp)
e13'  = App tv1 (Var tibt "f") [Tup tv2 [(Var it "a"), (Var bt "b")]]
e14  :: AExpr (Maybe Tp)
e14  = App tv1 (Var iitiibt "f") [(Tup tv2 [(Var it "a"), (Var it "b")]), (Tup tv2 [(Var it "b"), (Var it "c")])]
e14' :: AExpr (Maybe Tp)
e14' = App tv1 (Var iitiibt "f") [(Var it "a"), (Var it "b"), (Tup tv2 [(Var it "b"), (Var it "c")])]
e15  :: AExpr (Maybe Tp)
e15  = App tv1 (Var iitiibt "f") [(Var it "a"), (Var it "b"), (Var it "b"), (Var it "c")]
e15' :: AExpr (Maybe Tp)
e15' = App tv1 (Var iitiibt "f") [(Var it "a"), (Var it "b"), (Tup Nothing [(Var it "b"), (Var it "c")])]
e16  :: AExpr (Maybe Tp)
e16  = App tv1 (Var ibt "f") [(Var bt "a")]
e16' :: AExpr (Maybe Tp)
e16' = App tv1 (Var ibt "f") [(Var it "b")]
e16sib :: [Sibling]
e16sib = [[("b", i), ("a", b)], [("c", f), ("d", c)]]
   
-- | definition and execution of unittests
main :: IO ()
main = 
    do
        let

            
            -- definition of typetests
            typetests = [
                (App tv1 (Var ibt "a") [(Var it "b")], True),
                (App tv1 (Var ib_it "a") [(Var ibt "a")],True),
                (App tv2 (App tv1 (Var iibt "a") [(Var it "b")]) [(Var it "c")],True),
                (App tv1 (Var iibt "a") [(Var it "b"), (Var it "c")],True),
                (App tv1 (Var iibt "a") [(Var it "b")],True),
                (App tv1 (Var ibbt "a") [(Var bt "b"), (Var it "c")],False),
                (If tv1 (Var tv2 "a") (Var it "b") (Var it "c"),True),
                (App tv1 (Var lbbt "f") [(Var lbt "x")], True),
                (App tv1 (Var lbbt "f") [(Var bt "x"), (Var bt "y")], False)
                ]
                
            -- definition of transformation tests
            transformationtests = [
                -- swap 2 arguments in top level app
                (((e1,[permute]),1),e1'),
                -- swap 3 arguments in top level app
                (((e2,[permute]),1),e2'),
                -- swap args in nested app
                (((e3,[permute]),1),e3'),
                -- swap args in both toplevel and nested app
                (((e4,[permute]),2),e4'),
                -- listifying the second argument fixes the expression
                (((e5,[listify]),1),e5'),
                -- listifying the second argument and swapping the arguments fixes the expression
                (((e6,[listify, permute]),2),e6'),
                -- introducing an additional argument fixes the expression
                (((e7,[insertArgument]),1),e7'),
                -- deleting an argument fixes the expression
                (((e8,[deleteArgument]),1),e8'),
                -- allowing deletion of 2 arguments should produce more then one solution, but only one with a minimum number of transformation steps
                (((e9,[deleteArgument]),2),e9'),
                -- delete an argument, and insert a new one
                (((e10,[deleteArgument, insertArgument]),3),e10'),
                -- parenthesizing fixes the expression
                (((e11,[flattenApp]),1),e11'),
                -- removing parenthesis fixes the expression
                (((e12,[flattenAppRev]),1),e12'),
                -- tuple permutation
                (((e13,[permuteTuple]),1),e13'),
                -- currying
                (((e14,[curryTuple]),1),e14'),
                -- uncurrying
                (((e15,[uncurryTuple]),1),e15'),
                -- uncurrying
                (((e16,[sibling e16sib]),1),e16')
                
             ]
            -- execution of type tests
            typetestresults = zip typetests (map (wellTyped . fst) typetests)
            processedtyperesults = unzip (map (\((input, expr),res) -> printTypeTestResult input expr res) typetestresults)
            typeresultstrings = fst processedtyperesults
            typeresultnumsuccess = length (filter id (snd processedtyperesults))
            numtypetests = length typetests
               
            -- execution of transformation tests
            transtestresults = zip transformationtests (map (\(((expr, trans), depth), _) -> testRepair trans depth expr) transformationtests)
            processedtransresults = unzip (map (\((((expr, _),_), expect), result) -> printRepairTestResult  expr (Just expect) result) transtestresults)
            transresultstrings = fst processedtransresults
            transresultnumsuccess = length (filter id (snd processedtransresults))
            numtranstests = length transformationtests
            
        putStr (concat (typeresultstrings ++ transresultstrings))
        putStrLn ("Summary: ")
        putStrLn ("Type tests : " ++ (show typeresultnumsuccess) ++ " out of " ++ (show numtypetests) ++ " succeeded.")
        putStrLn ("Trans tests : " ++ (show transresultnumsuccess) ++ " out of " ++ (show numtranstests) ++ " succeeded.")
