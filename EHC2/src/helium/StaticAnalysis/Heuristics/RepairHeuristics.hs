{-| Module      :  RepairHeuristics
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    Heuristics that supply additional hints with a type error how a program
	can be corrected.
-}

module Helium.StaticAnalysis.Heuristics.RepairHeuristics where

import Top.Types
import Top.Interface.TypeInference
import Top.Interface.Qualification hiding (contextReduction)
import Top.Interface.Substitution
import Helium.Syntax.UHA (Range)
import Helium.Utils.OneLiner (OneLineTree)
import Top.Implementation.TypeGraph.Heuristic
import Top.Implementation.TypeGraph.Basics
import Top.Implementation.TypeGraph.ClassMonadic
import Data.Maybe
import Helium.StaticAnalysis.Messages.Messages (showNumber, ordinal, prettyAndList)
import Helium.StaticAnalysis.Heuristics.OnlyResultHeuristics
import Data.List
import Helium.StaticAnalysis.Miscellaneous.UHA_Source

-----------------------------------------------------------------------------

type Siblings = [[(String, TpScheme)]]

class MaybeImported a where
   maybeImportedName :: a -> Maybe String

siblingFunctions :: (MaybeImported info, HasTwoTypes info, WithHints info, HasTypeGraph m info) 
                       => Siblings -> Selector m info
siblingFunctions siblings = 
   Selector ("Sibling functions", f) where
  
 f pair@(edge, info) =
   case maybeImportedName info of 
      Nothing   -> return Nothing
      Just name
         | null candidates -> return Nothing
         | otherwise -> 
              doWithoutEdge pair $ 
              do (_, mtp) <- getSubstitutedTypes info -- Determine types of endpoints of edge when the edge is not present.
                 subPreds <- allSubstPredicates       -- Which predicates must also be satisfied?
                 case mtp of 
                    Nothing -> return Nothing
                    Just contextTp                         
                       | otherwise -> 
                            do fits <- mapM (schemeFits contextTp subPreds) (map snd candidates)
                               case [ s | (True, (s, _)) <- zip fits candidates ] of
                                  [] -> return Nothing
                                  siblings ->    -- TODO: put all siblings in the message
                                     let siblingsTextual = orList siblings
                                         hint = fixHint ("use "++siblingsTextual++" instead")
                                      in return $ Just
                                            (10,"Sibling(s) "++siblingsTextual++" instead of "++show name, [edge], hint info)
                                  
	   where
        orList :: [String] -> String
        orList [s]    = s
        orList (x:xs) = foldr (\x y-> x ++ ", " ++ y) ("or "++x) xs
        orList []     = "this should never occur"
        
        candidates = 
           let f list 
                  | name `notElem` map fst list = []
                  | otherwise = filter ( (name /=) . fst) list
           in concatMap f siblings
           
        schemeFits contextTp sps scheme = 
           do synonyms <- getTypeSynonyms 
              classEnv <- getClassEnvironment
              let magicNumber = 123456789
                  (ps, itp)   = split (snd (instantiate magicNumber scheme))
              return (unifiableInContext classEnv synonyms (map Just ps ++ sps) contextTp itp)
       			       
-----------------------------------------------------------------------------

class MaybeLiteral a where
   maybeLiteral :: a -> Maybe String  

siblingLiterals :: (HasTypeGraph m info, MaybeLiteral info, HasTwoTypes info, WithHints info) => Selector m info
siblingLiterals = 
   Selector ("Sibling literals", f) where

 f pair@(edge, info) =
   case maybeLiteral info of 
      Nothing      -> return Nothing
      Just literal ->

         doWithoutEdge pair $

            do synonyms <- getTypeSynonyms
               (_, mtp) <- getSubstitutedTypes info

               case (literal,mtp) of

                  ("Int", Just (TCon "Float"))
                       -> let hint = fixHint "use a float literal instead"
                          in return $ Just
                                (5, "Int literal should be a Float", [edge], hint info)

                  ("Float", Just (TCon "Int" ))
                       -> let hint = fixHint "use an int literal instead"
                          in return $ Just
                                (5, "Float literal should be an Int", [edge], hint info)

                  ("Char", Just (TApp (TCon "[]") (TCon "Char"))) 
                       -> let hint = fixHint "use a string literal instead"
                          in return $ Just
                                (5, "Char literal should be a String", [edge], hint info)

                  ("String", Just (TCon "Char"))   
                       -> let hint = fixHint "use a char literal instead"
                          in return $ Just
                                (5, "String literal should be a Char", [edge], hint info)

                  _ -> return Nothing 

-----------------------------------------------------------------------------

similarNegation :: (HasTypeGraph m info, MaybeNegation info, HasTwoTypes info, WithHints info) => Selector m info
similarNegation  =
   Selector ("Similar negation", f) where

 f pair@(edge, info) =   
   case maybeNegation info of
      Nothing            -> return Nothing
      Just isIntNegation ->

         doWithoutEdge pair $

            do synonyms <- getTypeSynonyms
               (_, mtp) <- getSubstitutedTypes info
               
               case mtp of
                  Just tp
                     | floatNegationEdge && intNegation && not floatNegation
                        -> let hint = fixHint "use int negation (-) instead"
                           in return $ Just
                                 (6, "Int negation instead of float negation", [edge], hint info)

                     | intNegationEdge && not intNegation && floatNegation 
                        -> let hint = fixHint "use float negation (-.) instead"
                           in return $ Just
                                 (6, "Float negation instead of int negation", [edge], hint info)
                       
                    where intNegation       = unifiable synonyms tp (intType .->. intType)
                          floatNegation     = unifiable synonyms tp (floatType .->. floatType)
                          intNegationEdge   = isIntNegation
                          floatNegationEdge = not isIntNegation                                         

                  _ -> return Nothing

-----------------------------------------------------------------------------

-- Clean up this function: split into smaller heuristics, and remove duplicated code.
applicationHeuristic :: (HasTypeGraph m info, MaybeApplication info, IsPattern info, HasTwoTypes info, WithHints info) => Selector m info
applicationHeuristic =
   Selector ("Application heuristics", f) where

 f pair@(edge, info) =
   case maybeApplicationEdge info of
      Nothing -> return Nothing
      Just (isBinary,tuplesForArguments) ->

       doWithoutEdge pair $

          do classEnv <- getClassEnvironment
             synonyms <- getTypeSynonyms                 
             (maybeFunctionType, maybeExpectedType) <- getSubstitutedTypes info
             subPreds <- allSubstPredicates     
             case (maybeFunctionType, maybeExpectedType) of
                                  
               (Just functionType, Just expectedType) 
                  -- can a permutation of the arguments resolve the type inconsistency
                  -- of course, I need at least two arguments
                  | length argumentPermutations == 1 && length (concat argumentPermutations) > 1 -> 
                       let p = head argumentPermutations
                       in 
                         if p==[1,0] && isBinary
                            then 
                                 let hint = fixHint "swap the two arguments"
                                 in return $ Just
                                       (3, "swap the two arguments", [edge], hint info)
                            else                                       
                                  let hint = fixHint "re-order arguments"
                                  in return $ Just
                                        (1, "application: permute with "++show p, [edge], hint info)
          
                  -- is there one particular argument that is inconsistent
                  | length incorrectArguments == 1  ->
                      do let (t1, t2) = getTwoTypes info
                         mtp <- substituteTypeSafe t1
                         case mtp of 
                            Nothing -> return Nothing
                            Just fullTp -> 
                               let i = head incorrectArguments
                                   {- bug fix 25 september 2003: don't forget to expand the type synonyms -}
                                   expandedTp  = expandType (snd synonyms) fullTp
                                   (source,tp) = tuplesForArguments !! i
                                   range       = rangeOfSource source
                                   oneLiner    = oneLinerSource source
                                   infoFun     = typeErrorForTerm (isBinary,isPatternApplication) i oneLiner (tp,expargtp) range
                                   expargtp    = fst (functionSpine expandedTp) !! i
                               in return $ Just 
                                     (3, "incorrect argument of application="++show i, [edge], infoFun info)

                  -- too many arguments are given
                  | maybe False (< numberOfArguments) maximumForFunction && not isPatternApplication ->
                       case typesZippedWithHoles of
                          -- there is only one possible set to remove arguments 
                          [is] | not isBinary && maybe True (>= 1) maximumForFunction
                              -> let hint = fixHint ("remove "++prettyAndList (map (ordinal True . (+1)) is)++" argument")
                                 in return $ Just
                                       (4, "too many arguments are given: "++show is, [edge], hint info)


                          _    -- the expression to which arguments are given does not have a function type
                               | maybe False (<= 0) maximumForFunction && not isBinary && not (isPattern info) ->                       
                                    let hint = becauseHint "it is not a function"
                                    in return $ Just
                                          (6, "not a function", [edge], hint info)

                               -- function used as infix that expects < 2 arguments
                               | maybe False (<= 1) maximumForFunction && isBinary && not (isPattern info) ->
                                    let hint = becauseHint "it is not a binary function"
                                    in return $ Just
                                          (6, "no binary function", [edge], hint info)

                          -- more than one or no possible set of arguments to be removed
                               | otherwise -> 
                                    let hint = becauseHint "too many arguments are given"
                                    in return $ Just
                                          (2, "too many arguments are given", [edge], hint info)
                                     
                  -- not enough arguments are given
                  | minimumForContext > numberOfArguments && not isPatternApplication && contextIsUnifiable ->
                       case typesZippedWithHoles of

                          [is] | not isBinary 
                              -> let hint = fixHint ("insert a "++prettyAndList (map (ordinal True . (+1)) is)++" argument")
                                 in return $ Just
                                       (4, "not enough arguments are given"++show is, [edge], hint info)

                          _   -> let hint = becauseHint "not enough arguments are given"
                                 in return $ Just
                                       (2, "not enough arguments are given", [edge], hint info)
             
                where unifiableTypes :: Tp -> Tp -> Bool
                      unifiableTypes = 
                         unifiableInContext classEnv synonyms subPreds
                
                      unifiableTypeLists :: Tps -> Tps -> Bool
                      unifiableTypeLists xs ys = 
                         unifiableTypes (tupleType xs) (tupleType ys)

                      -- number of arguments for this function application
                      numberOfArguments = length tuplesForArguments         
                                
                      -- (->) spines for function type and expected type for the given number of arguments
                      (functionArguments, functionResult) = functionSpineOfLength numberOfArguments (expandType (snd synonyms) functionType)
                      (expectedArguments, expectedResult) = functionSpineOfLength numberOfArguments (expandType (snd synonyms) expectedType)

                      -- (->) spines for function type and expected type ignoring the given number of arguments                      
                      (allFunctionArgs, allFunctionRes) = functionSpine functionType
                      (allExpectedArgs, allExpectedRes) = functionSpine expectedType
                      
                      -- maximum number of arguments for the function type (result type should not be polymorphic!)
                      --   e.g.: (a -> b) -> [a] -> [b]             yields (Just 2)
                      --         (a -> b -> b) -> b -> [a] -> b     yields Nothing
                      maximumForFunction = case functionSpine (expandType (snd synonyms) functionType) of
                                              (_, TVar _) -> Nothing
                                              (tps, _   ) -> Just (length tps)
                      
                      -- minimum number of arguments that should be applied to the function to meet the expected context type
                      minimumForContext = length allFunctionArgs + numberOfArguments - length allExpectedArgs
                      
                      -- is the context unifiable?
                      contextIsUnifiable    = unifiable synonyms expectedResult (snd (functionSpineOfLength minimumForContext functionType))

                      -- is there one argument in particular that is incorrect?                                  
                      incorrectArguments = [ i 
                                           | length functionArguments == length expectedArguments 
                                           , i <- [0..numberOfArguments-1]
                                           , not (unifiableTypes (functionArguments !! i) (expectedArguments !! i))
                                           , unifiableTypeLists (functionResult : deleteIndex i functionArguments) 
                                                                (expectedResult : deleteIndex i expectedArguments)
                                           ]
                         
                      -- is there a permutation of the arguments that resolves the type inconsistency?
                      argumentPermutations = [ p 
                                             | length functionArguments == length expectedArguments 
                                             , p <- take heuristics_MAX (permutationsForLength numberOfArguments)
                                             , unifiableTypeLists (functionResult : functionArguments) 
                                                                  (expectedResult : permute p expectedArguments) 
                                             ]                                                            

                      -- at which locations should an extra argument be inserted?
                      typesZippedWithHoles  = [ is 
                                              | (is,zl) <- take heuristics_MAX (zipWithHoles allFunctionArgs allExpectedArgs)
                                              , let (as,bs) = unzip zl
                                              , unifiableTypeLists (allFunctionRes : as) 
                                                                   (allExpectedRes : bs)
                                              ]                     
                      
                      isPatternApplication = isPattern info

               (Just functionType, _)
               
                  -- the expression to which arguments are given does not have a function type
                  | maybe False (<= 0) maximumForFunction && not isBinary && not (isPattern info) ->                       
                       let hint = becauseHint "it is not a function"
                       in return $ Just
                             (6, "not a function", [edge], hint info)

                  -- function used as infix that expects < 2 arguments
                  | maybe False (<= 1) maximumForFunction && isBinary && not (isPattern info) ->
                       let hint = becauseHint "it is not a binary function"
                       in return $ Just
                             (6, "no binary function", [edge], hint info)

                  -- too many arguments are given
                  | maybe False (< length tuplesForArguments) maximumForFunction && not (isPattern info) ->
                       let hint = becauseHint "too many arguments are given"
                       in return $ Just
                             (2, "too many arguments are given", [edge], hint info)
                                     
                where -- copy/paste :-(
                      maximumForFunction   = case functionSpine (expandType (snd synonyms) functionType) of
                                                (_, TVar _) -> Nothing
                                                (tps, _   ) -> Just (length tps)                       
               _ -> return Nothing

-----------------------------------------------------------------------------

class IsTupleEdge a where
   isTupleEdge :: a -> Bool

tupleHeuristic :: (HasTypeGraph m info, IsTupleEdge info, HasTwoTypes info, WithHints info) => Selector m info
tupleHeuristic =
   Selector ("Tuple heuristics", f) where

 f pair@(edge, info)    
   | not (isTupleEdge info) = return Nothing
   | otherwise              =
   
   doWithoutEdge pair $ 
   
      do classEnv <- getClassEnvironment
         synonyms <- getTypeSynonyms                         
         (mTupleTp, mExpectedTp) <- getSubstitutedTypes info
         subPreds <- allSubstPredicates             
         case (fmap leftSpine mTupleTp,fmap leftSpine mExpectedTp) of 
            
          (Just (TCon s,tupleTps),Just (TCon t,expectedTps)) | isTupleConstructor s && isTupleConstructor t ->
            case compare (length tupleTps) (length expectedTps) of
            
               EQ -> -- try if a permutation can make the tuple types equivalent
                  let perms = take heuristics_MAX (permutationsForLength (length tupleTps))
                      notUnifiable = not (unifiableInContext classEnv synonyms subPreds (tupleType tupleTps) (tupleType expectedTps))
                      test perm = 
                         let t1 = tupleType tupleTps
                             t2 = tupleType (permute perm expectedTps)
                         in unifiableInContext classEnv synonyms subPreds t1 t2
                  in case filter test perms of
                        p:_ | notUnifiable -> -- a permutation is possible!
                           let hint = fixHint "re-order elements of tuple"
                           in return $ Just 
                                   (4, "tuple: permute with "++show p ++ show (mTupleTp, mExpectedTp), [edge], hint info)
                        _ -> return Nothing
                                 
               compare -> case [ is 
                               | (is,zl) <- take heuristics_MAX (zipWithHoles tupleTps expectedTps)
                               , let (xs, ys) = unzip zl in unifiable synonyms (tupleType xs) (tupleType ys)
                               ] of
                       [is] -> case compare of
                                 LT -> let hint = fixHint ("insert a "++prettyAndList (map (ordinal True. (+1)) is)++" element to the tuple")
                                       in return $ Just 
                                             (4, "tuple:insert "++show is, [edge], hint info)
                                 GT -> let hint = fixHint ("remove "++prettyAndList (map (ordinal True . (+1)) is)++" element of tuple")
                                       in return $ Just 
                                             (4, "tuple:remove "++show is, [edge], hint info)
                       _    -> let hint = becauseHint ("a "++show (length tupleTps)++"-tuple does not match a "++show (length expectedTps)++"-tuple")
                               in return $ Just 
                                     (2, "different sizes of tuple", [edge], hint info)
          _ -> return Nothing  

-----------------------------------------------------------------------------

class IsFunctionBinding a where
   isExplicitlyTyped    :: a -> Bool
   maybeFunctionBinding :: a -> Maybe Int

fbHasTooManyArguments :: (HasTypeGraph m info, IsFunctionBinding info, HasTwoTypes info, WithHints info) => Selector m info
fbHasTooManyArguments =
   Selector ("Function binding heuristics", f) where

 f (edge, info)   
   | not (isExplicitlyTyped info) = return Nothing
   | otherwise                    =

      do synonyms <- getTypeSynonyms
         let (t2,t1)         = getTwoTypes info
             maximumExplicit = arityOfTp (expandType (snd synonyms) t1)
             tvar            = if null (ftv t2) then (-1) else head (ftv t2) -- !!!!!!!!!!!!!!!!!!!
   
         edgeList <- edgesFrom (VertexId tvar)      
         let maybeNumberOfPatterns = 
                case [ i | Just i <- map (\(_, info) -> maybeFunctionBinding info) edgeList ] of 
                   [i] -> Just i
                   _   -> Nothing
                                      
         case maybeNumberOfPatterns of
            Just n | n > maximumExplicit -> 
               let msg = "the function binding has "++prettyPat n++", but its type signature "++prettyArg maximumExplicit
                   prettyPat i = if i == 1 then "1 pattern" else show i++" patterns"
                   prettyArg 0 = "does not allow patterns"
                   prettyArg n = "allows at most "++show n
                   hint = becauseHint msg
               in return $ Just 
                     (8, "function binding has too many arguments", [edge], hint info)
            _ -> return Nothing

-----------------------------------------------------------------------------

class IsExprVariable a where
   isExprVariable          :: a -> Bool
   isEmptyInfixApplication :: a -> Bool

variableFunction :: (HasTypeGraph m info, IsExprVariable info, MaybeApplication info, HasTwoTypes info, WithHints info) => Selector m info
variableFunction =
   Selector ("Variable function", f) where

 f pair@(edge, info)      
   | not (isExprVariable info)
        = return Nothing
   | otherwise 
        = doWithoutEdge pair $ 
            
           do synonyms   <- getTypeSynonyms
              (mt1, mt2) <- getSubstitutedTypes info
              
              -- is this variable involved in an application?
              let EdgeId v1 v2 _ = edge
              edges1 <- edgesFrom v1
              edges2 <- edgesFrom v2
              let f ((EdgeId v1 v2 _), _) = [v1,v2]
                  special = concatMap f (filter (isEmptyInfixApplication . (\(_, info) -> info)) (edges1 ++ edges2)) \\ [v1,v2]
              edges3 <- mapM edgesFrom special
              let isApplicationEdge = isJust . maybeApplicationEdge
                  application = any (\(_, info) -> isApplicationEdge info) (edges1 ++ edges2 ++ concat edges3)                                                               
             
              case (mt1, mt2) of
                 (Just functionType, Just expectedType) | not application -> 
                    let maxArgumentsForFunction = length (fst (functionSpine functionType))
                        minArgumentsForContext  = maxArgumentsForFunction - length (fst (functionSpine expectedType)) 
                        contextIsUnifiable      = unifiable synonyms 
                                                     (snd $ functionSpineOfLength minArgumentsForContext functionType)
                                                     expectedType
                    in if minArgumentsForContext <= 0 || not contextIsUnifiable
                         then return Nothing
                         else let hint = fixHint ("insert "++showNumber minArgumentsForContext++" argument"++
                                              if minArgumentsForContext <= 1 then "" else "s")
                              in return $ Just 
                                    (4, "insert arguments to function variable", [edge], hint info)
                 _ -> return Nothing

-----------------------------------------------------------------------------

class MaybeUnaryMinus a where
   maybeUnaryMinus :: a -> Maybe (Either Int Float)

unaryMinus :: (HasTypeGraph m info, MaybeApplication info, MaybeUnaryMinus info, HasTwoTypes info, WithHints info) => Bool -> Selector m info
unaryMinus overloading = 
   Selector ("Unary minus", f) where

 f pair@(edge, info) =
   case maybeApplicationEdge info of
      Just (isInfix, tuplesForArguments) | isInfix && length tuplesForArguments == 2 -> 
         case maybeUnaryMinus info of
            Just someLiteral ->
               doWithoutEdge pair $ 
               do synonyms <- getTypeSynonyms 
                  let leftBeta = snd (head tuplesForArguments)
                  leftType <- substituteTypeSafe leftBeta
                  (_, mt2) <- getSubstitutedTypes info
                  let contextType = fmap (snd . functionSpineOfLength 2 . expandType (snd synonyms)) mt2
                  case (someLiteral, leftType, contextType) of
                     (Left int, Just leftTp, Just contextTp) 
                        | unifiable synonyms leftTp (intType .->. contextTp) -> 
                             let hint = possibleHint ("Insert parentheses to negate the int literal: (-"++show int++")")
                             in return $ Just 
                                   (5, "Unary minus for int", [edge], hint info)
                     (Right float, Just leftTp, Just contextTp) 
                        | unifiable synonyms leftTp (floatType .->. contextTp) && not overloading -> 
                             let hint = possibleHint ("Insert parentheses to negate the float literal: (-."++show float++")")
                             in return $ Just 
                                   (5, "Unary minus for float", [edge], hint info)
                        | unifiable synonyms leftTp (floatType .->. contextTp) && overloading -> 
                             let hint = possibleHint ("Insert parentheses to negate the float literal: (-"++show float++")")
                             in return $ Just 
                                   (5, "Unary minus for float (overloading)", [edge], hint info)
                     _ -> return Nothing
            _ -> return Nothing
      _ -> return Nothing
                       
-----------------------------------------------------------------------------
-- REST 

heuristics_MAX = 120 :: Int

zipWithHoles :: [a] -> [b] -> [ ( [Int] , [(a,b)] ) ] 
zipWithHoles = rec 0 where

   rec i [] bs = [ (take (length bs) [i..] , []) ]
   rec i as [] = [ (take (length as) [i..] , []) ]
   rec i (a:as) (b:bs) = case compare (length as) (length bs) of
         LT -> [ (  is,(a,b):zl) | (is,zl) <- rec (i+1) as     bs ]
            ++ [ (i:is,      zl) | (is,zl) <- rec (i+1) (a:as) bs ]
         EQ -> [ ([],zip (a:as) (b:bs)) ] 
         GT -> [ (  is,(a,b):zl) | (is,zl) <- rec (i+1) as bs     ]
            ++ [ (i:is,      zl) | (is,zl) <- rec (i+1) as (b:bs) ]

type Permutation = [Int]

permutationsForLength :: Int -> [Permutation]
permutationsForLength 0 = [ [] ]
permutationsForLength i = [ ys | xs <- permutationsForLength (i-1), ys <- insertSomewhere (i-1) xs ]
  where
	insertSomewhere i []     = [ [i] ]
	insertSomewhere i (x:xs) = (i:x:xs) : map (x:) (insertSomewhere i xs)
         
deleteIndex :: Int -> [a] -> [a]
deleteIndex _ []     = []
deleteIndex 0 (a:as) = as
deleteIndex i (a:as) = a : deleteIndex (i-1) as

permute :: Permutation -> [a] -> [a]
permute is as = map (as !!) is

class WithHints a where
   addHint          :: String -> String -> a -> a
   typeErrorForTerm :: (Bool,Bool) -> Int -> OneLineTree -> (Tp,Tp) -> Range -> a -> a

fixHint, becauseHint, possibleHint :: WithHints a => String -> a -> a
fixHint      = addHint "probable fix"
becauseHint  = addHint "because"
possibleHint = addHint "possible fix"

------------------------------------------------------------------

unifiableInContext :: ClassEnvironment -> OrderedTypeSynonyms -> [Maybe Predicate] -> Tp -> Tp -> Bool
unifiableInContext classEnv synonyms mps t1 t2
   | any isNothing mps = False
   | otherwise = 
        case mguWithTypeSynonyms synonyms t1 t2 of
           Left _         -> False
           Right (_, sub) ->
              let ps        = catMaybes mps
                  (_, errs) = contextReduction synonyms classEnv (sub |-> ps)
              in null errs

allSubstPredicates :: (HasTypeGraph m info) => m [Maybe Predicate]
allSubstPredicates =
   do synonyms <- getTypeSynonyms
      allPreds <- allQualifiers
      let f (Predicate s tp) = 
             do mtp <- substituteTypeSafe tp
                return (fmap (Predicate s) mtp)
      mapM f allPreds
      
predicatesFit :: (HasTypeGraph m info) => Predicates -> m Bool
predicatesFit ps = 
   do bs <- mapM predicateFits ps
      return (and bs)
        
predicateFits :: (HasTypeGraph m info) => Predicate -> m Bool        
predicateFits (Predicate s tp) =
   do synonyms <- getTypeSynonyms
      classEnv <- getClassEnvironment
      mtp      <- substituteTypeSafe tp
      case mtp of
         Nothing  -> return False
         Just tp' -> 
            let (_, errs) = contextReduction synonyms classEnv [Predicate s tp']
            in return (null errs)