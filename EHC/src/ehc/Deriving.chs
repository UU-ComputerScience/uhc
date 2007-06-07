%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deriving info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95 module {%{EH}Deriving} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Gam},{%{EH}Ty},{%{EH}Core},{%{EH}Core.Utils})
%%]

%%[95 import(qualified Data.Map as Map,Data.List,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Table for each derivable field of each derivable class: type as well as codegen info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95 export(DerivClsFld(..),DerivClsMp)
data DerivClsFld
  = DerivClsFld
      { dcfNm                       :: !HsName                                  -- name of field
      , dcfTy                       :: !Ty                                      -- type of field
      , dcfInitialArgL              :: ![HsName]                                -- initial arguments for function, passed to dcfFoldSubsCExpr
      , dcfInitialSubArgL           :: DataTagInfo -> [CExpr]                   -- initial arguments for constituents
      , dcfNrOmitTailArg            :: !Int                                     -- nr of args not passed, i.e. how much to omit for partial app
      , dcfFoldSubsCExpr            :: UID										-- construct out of constituents
                                       -> RCEEnv
                                       -> DataTagInfo							-- this tag info
                                       -> (Int,Int)								-- (index (a la toEnum), nr of alts)
                                       -> [HsName]								-- names of initial args (usually same as dcfInitialArgL)
                                       -> [CExpr]								-- constituents
                                       -> CExpr
      , dcfNoArgSubsCExpr           :: [(DataTagInfo,[CExpr])]					-- all tag info + subs
                                       -> CExpr                                 -- when class member takes no args
      , dcfAllTagLtCExpr            :: !CExpr                                   -- when tags are < previous
      , dcfAllTagGtCExpr            :: !CExpr                                   -- when tags are > previous
      }

type DerivClsMp = Map.Map HsName (AssocL HsName DerivClsFld)
%%]

The table is dependent on builtin names, stored in EHCOpts.

%%[95 export(mkDerivClsMp)
mkDerivClsMp :: EHCOpts -> ValGam -> DataGam -> DerivClsMp
mkDerivClsMp opts valGam dataGam
  = Map.unionsWith (++)
    $ map (uncurry Map.singleton) 
    $ [
      -- Eq((==))
        mk ehbnClassEq ehbnClassEqFldEq
           []
           (const [])
           0
           (\_ _ _ _ _ vs
                -> case vs of
                     [] -> true
                     _  -> foldr1 and vs
           )
           (const undef)
           false false
      
      -- Ord(compare)
      , mk ehbnClassOrd ehbnClassOrdFldCompare
           []
           (const [])
           0
           (\uniq env _ _ _ vs
                -> case vs of
                     [] -> eq
                     _  -> foldr1 (\l r -> mkCExprStrictSatCase env (Just nStrict) l
                                             [ CAlt_Alt (mkCPatCon (orderingTag eqNm) 0 Nothing) r
                                             , CAlt_Alt (mkCPatCon (orderingTag ltNm) 0 Nothing) lt
                                             , CAlt_Alt (mkCPatCon (orderingTag gtNm) 0 Nothing) gt
                                             ]
                                  ) vs
                        where n = mkHNm uniq
                              nStrict = hsnSuffix n "!"
           )
           (const undef)
           gt lt
      
      -- Enum(fromEnum,succ,pred)
      , mk ehbnClassEnum ehbnClassEnumFldFromEnum
           []
           (const [])
           0
           (\_ env _ (altInx,_) _ []
                -> CExpr_Int altInx
           )
           (const undef)
           undef undef
      , mk ehbnClassEnum ehbnClassEnumFldSucc
           []
           (const [])
           0
           (\_ env dti (altInx,nrOfAlts) _ []
                -> if altInx < nrOfAlts  - 1
                   then CExpr_Int $ altInx + 1
                   else cerror opts $ "cannot succ last constructor: " ++ show (dtiConNm dti)
           )
           (const undef)
           undef undef
      , mk ehbnClassEnum ehbnClassEnumFldPred
           []
           (const [])
           0
           (\_ env dti (altInx,_) _ []
                -> if altInx > 0
                   then CExpr_Int $ altInx - 1
                   else cerror opts $ "cannot pred first constructor: " ++ show (dtiConNm dti)
           )
           (const undef)
           undef undef
      
      -- Bounded(maxBound,minBound)
      , mk ehbnClassBounded ehbnClassBoundedFldMaxBound
           []
           (const [])
           0
           (\_ _ _ _ _ _
                -> undef
           )
           (\dtiSubs
                -> let (dti,subs) = last dtiSubs
                   in  CExpr_Tup (dtiCTag dti) `mkCExprApp` subs
           )
           undef undef
      , mk ehbnClassBounded ehbnClassBoundedFldMinBound
           []
           (const [])
           0
           (\_ _ _ _ _ _
                -> undef
           )
           (\dtiSubs
                -> let (dti,subs) = head dtiSubs
                   in  CExpr_Tup (dtiCTag dti) `mkCExprApp` subs
           )
           undef undef
      
      -- Show(showsPrec)
      , mk ehbnClassShow ehbnClassShowFldShowsPrec
           [precDepthNm]
           (\dti -> [CExpr_Int $ maybe (fixityMaxPrio + 1) (+1) $ dtiMbFixityPrio $ dti])
           0
           (\_ _ dti _ [dNm] vs
                -> let mk needParen v = mkCExprApp (CExpr_Var $ fn ehbnPrelShowParen) [needParen,v]
                   in  case (vs,dtiMbFixityPrio dti) of
                         ([],_)
                           -> showString $ tag2str (dtiCTag dti)
                         ([v1,v2],Just p)
                           -> mk (cgtint opts (CExpr_Var dNm) p)
                              $ foldr1 compose ([v1] ++ [ showString $ " " ++ tag2str (dtiCTag dti) ++ " " ] ++ [v2])
                         _ -> mk (cgtint opts (CExpr_Var dNm) fixityMaxPrio)
                              $ foldr1 compose ([showString $ tag2str (dtiCTag dti) ++ " "] ++ intersperse (showString " ") vs)
           )
           (const undef)
           undef undef
      ]
  where mk c f as asSubs omTl cAllMatch cNoArg cLT cGT
          = (c', [mkf f])
          where c' = fn c
                mkf f = (f', DerivClsFld f' t as asSubs omTl cAllMatch cNoArg cLT cGT)
                      where f' = fn f
                            (t,_) = valGamLookupTy f' valGam
        fn f  = f $ ehcOptBuiltinNames opts
        false = CExpr_Var $ fn ehbnBoolFalse
        true  = CExpr_Var $ fn ehbnBoolTrue
        and l r = (CExpr_Var $ fn ehbnBoolAnd) `mkCExprApp` [l,r]
        compose l r = (CExpr_Var $ fn ehbnPrelCompose) `mkCExprApp` [l,r]
        showString s = (CExpr_Var $ fn ehbnPrelShowString) `mkCExprApp` [cstring opts s]
        eqNm = fn ehbnDataOrderingAltEQ
        ltNm = fn ehbnDataOrderingAltLT
        gtNm = fn ehbnDataOrderingAltGT
        eq = CExpr_Var eqNm
        lt = CExpr_Var ltNm
        gt = CExpr_Var gtNm
        precDepthNm = mkHNm "d"
        undef = cundefined opts
        tag2str = show . hsnQualified . ctagNm
        orderingTag conNm
          = dtiCTag
            $ dgiDtiOfCon conNm
            $ panicJust "mkDerivClsMp.dataGamLookup"
            $ dataGamLookup (fn ehbnDataOrdering) dataGam
%%]

