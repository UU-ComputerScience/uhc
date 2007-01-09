%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deriving info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95 module {%{EH}Deriving} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Gam},{%{EH}Ty},{%{EH}Core},{%{EH}Core.Utils})
%%]

%%[95 import(qualified Data.Map as Map,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Table for each derivable field of each derivable class: type as well as codegen info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95 export(DerivClsFld(..),DerivClsMp)
data DerivClsFld
  = DerivClsFld
      { dcfNm 						:: HsName									-- name of field
      , dcfTy 						:: Ty										-- type of field
      , dcfAllTagEqCExpr 			:: UID -> RCEEnv -> [CExpr] -> CExpr		-- when tags are equal how to combine/fold result of constituents
      , dcfAllTagLtCExpr 	   		:: CExpr									-- when tags are < previous
      , dcfAllTagGtCExpr 	   		:: CExpr									-- when tags are > previous
      }

type DerivClsMp = Map.Map HsName (AssocL HsName DerivClsFld)
%%]

The table is dependent on builtin names, stored in EHCOpts.

%%[95 export(mkDerivClsMp)
mkDerivClsMp :: EHCOpts -> ValGam -> DataGam -> DerivClsMp
mkDerivClsMp opts valGam dataGam
  = Map.fromList
      [ mk ehbnClassEq [ehbnClassEqFldEq]
           (\_ _ vs
                -> case vs of
                     [] -> true
                     _  -> foldr1 (\l r -> CExpr_Var (ehbnBoolAnd $ ehcOptBuiltinNames opts) `mkCExprApp` [l,r]) vs
           )
           false false
      , mk ehbnClassOrd [ehbnClassOrdFldCompare]
           (\uniq env vs
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
           gt lt
      ]
  where mk c fs cAllMatch cLT cGT
          = (cn, map mkf fs)
          where cn = c $ ehcOptBuiltinNames opts
                mkf f = (fn, DerivClsFld fn t cAllMatch cLT cGT)
                      where fn    = f $ ehcOptBuiltinNames opts
                            (t,_) = valGamLookupTy fn valGam
        false = CExpr_Var $ ehbnBoolFalse $ ehcOptBuiltinNames opts
        true  = CExpr_Var $ ehbnBoolTrue  $ ehcOptBuiltinNames opts
        eqNm = ehbnDataOrderingAltEQ $ ehcOptBuiltinNames opts
        ltNm = ehbnDataOrderingAltLT $ ehcOptBuiltinNames opts
        gtNm = ehbnDataOrderingAltGT $ ehcOptBuiltinNames opts
        eq = CExpr_Var eqNm
        lt = CExpr_Var ltNm
        gt = CExpr_Var gtNm
        orderingTag conNm
          = dtiCTag
            $ dgiDtiOfCon conNm
            $ panicJust "mkDerivClsMp.dataGamLookup"
            $ dataGamLookup (ehbnDataOrdering $ ehcOptBuiltinNames opts) dataGam
%%]

