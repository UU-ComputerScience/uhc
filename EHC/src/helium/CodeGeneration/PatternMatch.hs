{-| Module      :  PatternMatch
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.CodeGeneration.PatternMatch(patternToCore, patternsToCore, nextClauseId, freshIds) where

import qualified Lvm.Core.Core as Core
import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils
import Helium.Syntax.UHA_Range
import Lvm.Common.Id
import Char
import Helium.Utils.Utils
import Helium.CodeGeneration.CoreUtils

patternsToCore :: [(Id, Pattern)] -> Core.Expr -> Core.Expr
patternsToCore nps continue = fst (patternsToCore' nps continue 0)

patternsToCore' :: [(Id, Pattern)] -> Core.Expr -> Int -> (Core.Expr, Int)
patternsToCore' [] continue nr = (continue, nr)
patternsToCore' (np:nps) continue nr =
    let (expr, nr') = patternsToCore' nps continue nr
    in patternToCore' np expr nr'
    
patternToCore :: (Id, Pattern) -> Core.Expr -> Core.Expr
patternToCore np continue = fst (patternToCore' np continue 0)
    
withNr nr e = (e, nr)

patternToCore' :: (Id, Pattern) -> Core.Expr -> Int -> (Core.Expr, Int)
patternToCore' (name, pat) continue nr = 
    case pat of
        -- let x = _u1 in ...
        Pattern_Variable _ n -> withNr nr $
            if name == wildcardId || name == idFromName n then
                continue
            else 
                let_ (idFromName n) (Core.Var name) continue
        
        -- case _u1 of C _l1 _l2 -> ...
        --             _         -> _next
        Pattern_Constructor _ n ps -> 
            let 
                (ids, nr') =
                    if all isSimple ps then 
                        (map getIdOfSimplePattern ps, nr)
                    else 
                        freshIds' "l$" nr (length ps)
                (expr, nr'') =
                    patternsToCore' (zip ids ps) continue nr'
            in withNr nr'' $
                case_ name
                [ Core.Alt 
                    (Core.PatCon (Core.ConId (idFromName n)) ids) 
                    expr
                ]

        -- case _u1 of _l1 : _l2 -> ...
        --             _         -> _next
        Pattern_InfixConstructor _ p1 n p2 ->
            let ie = internalError "PatternMatch" "patternToCore'" "shouldn't look at range"
            in patternToCore' (name, Pattern_Constructor ie n [p1, p2]) continue nr
                
        Pattern_Parenthesized _ p ->
            patternToCore' (name, p) continue nr

        -- let n = _u1 in ...
        Pattern_As _ n p -> 
            let (expr, nr') = patternToCore' (name, p) continue nr
            in withNr nr' $
                let_ 
                    (idFromName n) (Core.Var name) 
                    expr

        Pattern_Wildcard _ -> withNr nr $
            continue
                
        -- case _u1 of 42 -> ...
        --             _  -> _next        

        Pattern_Literal _ l ->  
            case l of
                Literal_Int _ i -> withNr nr $
                    case_ name [ Core.Alt (Core.PatLit (Core.LitInt (read i))) continue ]
                Literal_Char r c -> withNr nr $
                    case_ name 
                    [ Core.Alt  
                        (Core.PatLit 
                            (Core.LitInt (ord (read ("'" ++ c ++ "'"))))
                        )
                        continue 
                    ]
                Literal_Float _ f -> withNr nr $
                    if_ (var "$primEqFloat" `app_` float f `app_` Core.Var name)
                        continue
                        (Core.Var nextClauseId)
-- !!! if we would have MATCHFLOAT instruction it could be: 
--  case_ name [ Core.Alt (Core.PatLit (Core.LitDouble (read f))) continue ]
                Literal_String _ s -> 
                    patternToCore' 
                        ( name
                        , Pattern_List noRange 
                            (map (Pattern_Literal noRange . Literal_Int noRange . show . ord) characters) 
                        )
                        continue
                        nr
                  where
                    characters = read ("\"" ++ s ++ "\"") :: String
            
        Pattern_List _ ps -> 
            patternToCore' (name, expandPatList ps) continue nr
        
        Pattern_Tuple _ ps ->
            let
                (ids, nr') =
                    if all isSimple ps then 
                        (map getIdOfSimplePattern ps, nr)
                    else 
                        freshIds' "l$" nr (length ps)
                (expr, nr'') = 
                    patternsToCore' (zip ids ps) continue nr'
            in withNr nr'' $
                case_ name
                [ Core.Alt 
                    (Core.PatCon (Core.ConTag 0 (length ps)) ids) 
                    expr
                ]
            
        
        Pattern_Negate _ (Literal_Int r v) -> 
            patternToCore' 
                (name, Pattern_Literal r (Literal_Int r neg))
                continue
                nr
            where
                neg = show (-(read v :: Int))

        Pattern_Negate _ (Literal_Float r v) -> 
            patternToCore'
                (name, Pattern_Literal r (Literal_Float r neg))
                continue
                nr
            where
                neg = show (-(read v :: Float))
            
        Pattern_NegateFloat _ (Literal_Float r v) -> 
            patternToCore'
                (name, Pattern_Literal r (Literal_Float r neg))
                continue
                nr
            where
                neg = show (-(read v :: Float))

        -- ~p  ====>
        --   let x = case _u1 of p -> x
        --       y = case _u1 of p -> y   (for each var in p)
        --   in continue
        Pattern_Irrefutable _ p -> 
            let vars = map idFromName (patternVars p)
            in withNr nr $ foldr 
                (\v r -> let_ v (patternToCore (name, p) (Core.Var v)) r)
                continue
                vars
        
        _ -> internalError "PatternMatch" "patternToCore'" "unknown pattern kind"

-- [1, 2, 3] ==> 1 : (2 : (3 : [] ) )
expandPatList :: [Pattern] -> Pattern
expandPatList [] = 
    Pattern_Constructor noRange (Name_Special noRange [] "[]") [] -- !!!Name
expandPatList (p:ps) =
    Pattern_InfixConstructor 
        noRange 
        p
        (Name_Identifier noRange [] ":")  -- !!!Name
        (expandPatList ps)
    
isSimple :: Pattern -> Bool
isSimple p = 
    case p of
        Pattern_Variable _ _ -> True
        Pattern_Wildcard _ -> True
        _ -> False

getIdOfSimplePattern :: Pattern -> Id
getIdOfSimplePattern p =
    case p of
        Pattern_Variable _ n -> idFromName n
        Pattern_Wildcard _ -> wildcardId
        _ -> internalError "PatternMatch" "getIdOfSimplePattern" "not a simple pattern"
        
freshIds :: String -> Int -> [Id]
freshIds prefix number = fst (freshIds' prefix 0 number)

freshIds' :: String -> Int -> Int -> ([Id], Int)
freshIds' prefix start number = 
    ( take number [ idFromString (prefix ++ show i) | i <- [start..] ]
    , number + start
    )

nextClauseAlternative :: Core.Alt
nextClauseAlternative =
    Core.Alt Core.PatDefault (Core.Var nextClauseId)

( wildcardId :  nextClauseId : [] ) = map idFromString $
  "_"        : "nextClause$" : [] 

case_ :: Id -> [Core.Alt] -> Core.Expr
case_ id alts = 
    Core.Let 
        (Core.Strict (Core.Bind id (Core.Var id)))      -- let! id = id in
        (Core.Match id (alts++[nextClauseAlternative])) -- match id { alt; ...; alt; _ -> _nextClause }
    
