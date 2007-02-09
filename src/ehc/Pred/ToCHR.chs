%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[9 module {%{EH}Pred.ToCHR} import({%{EH}CHR},{%{EH}CHR.Constraint})
%%]

%%[9 import(qualified Data.Set as Set,Data.Maybe)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9

%%]

sPreds1 = flip sPred s1

scopedTrans1 clsDecls insts =
  let  (assumeChrs, instChrs) = genScopedChrs clsDecls insts
       simplChrs  =  concatMap (genClassSimplChrs' False assumeChrs) clsDecls
       scoperule  =  [Prove (SPred p1 s1)] 
                     ==> [Reduction (SPred p1 s1) ByScope [(SPred p1 s2)],  Prove (SPred p1 s2)]
                     |> s2 `assignParentScope` s1 
  in   scoperule :  assumeChrs ++ instChrs ++ simplChrs 

scopedTrans2 clsDecls insts =
  let  (assumeChrs, instChrs) = genScopedChrs clsDecls insts
       simplChrs  = concatMap (genClassSimplChrs' True assumeChrs) clsDecls
       scopeProve = [Prove (SPred p1 s1), Prove (SPred p1 s2)] 
                    ==> [Prove (SPred p1 s3), Reduction (SPred p1 s1) ByScope [(SPred p1 s3)]]
                    |> assignCommonScope s1 s2 s3   
       scopeAssum = [Prove (SPred p1 s1), Assume (SPred p1 s2)] 
                    ==> [Reduction (SPred p1 s1) ByScopeA [(SPred p1 s3)]]
                    |> assignCommonScope s1 s2 s3                            
  in   scopeProve : scopeAssum : assumeChrs ++ instChrs ++ simplChrs 
                        
genClassSimplChrs' scopeRules rules (context, head, infos) =
  let superClasses = chrSolve' rules (map (Assume . sPreds1) context)
      graph        = foldr addReduction emptyAGraph superClasses 

      mapTrans reds subClass = concatMap (transClosure reds subClass)

      transClosure reds par (info, pr@(Pred p@(SPred super _))) =
        let superRule  = [Prove (sPred head s1), Prove p] ==> Prove (sPred head s1) : reds'
            
            scopeRule1 = [Prove (sPred head s1), Prove (SPred super s2)] 
                         ==> [Prove (sPred head s3), Reduction (sPred head s1) ByScope [(sPred head s3)]]
                         |> assignCommonScope s1 s2 s3
                         
            scopeRule2 = [Prove (sPred head s2), Prove (SPred super s1)] 
                         ==> [Prove (SPred super s3), Reduction (SPred super s1) ByScope [(SPred super s3)]]
                         |> assignCommonScope s1 s2 s3
                         
            reds'  = Reduction p info [par] : reds
                         
            rules = mapTrans reds' p (predecessors graph pr)             
                        
         in if scopeRules
            then superRule : scopeRule1 : scopeRule2 : rules 
            else superRule : rules

  in mapTrans [] (sPreds1 head ) (zip infos (map (Pred . sPreds1) context))

genScopedChrs clsDecls insts =
  let assumeChrs = mapMaybe genAssumeChrs clsDecls 
      instChrs   = genInstanceChrs insts
  in  (assumeChrs, instChrs)

genAssumeChrs ([]     ,  _  , _    ) = Nothing
genAssumeChrs (context, head, infos) =
  let super sClass info = [Assume (sPreds1 sClass), Reduction (sPreds1 sClass) info [sPreds1 head]]
  in  Just ([Assume (sPreds1 head)] ==> concat (zipWith super context infos))

genInstanceChrs insts = map genInstanceChr insts

genInstanceChr (context, hd, i, s) =
  let  constraint = sPreds1 hd
       body = map sPreds1 context
  in   [Prove constraint] ==>
       Reduction constraint i body : map Prove body
       |> s1 `visibleIn` s

