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
      { dcfNm                       :: HsName                                   -- name of field
      , dcfTy                       :: Ty                                       -- type of field
      , dcfInitialArgL              :: [HsName]                                 -- initial arguments for function, passed to dcfAllTagEqCExpr
      , dcfInitialSubArgL           :: [CExpr]                                  -- initial arguments for constituents
      , dcfNrOmitTailArg            :: Int                                      -- nr of args not passed, i.e. how much to omit for partial app
      , dcfAllTagEqCExpr            :: UID -> RCEEnv -> CTag -> [HsName] -> [CExpr] -> CExpr
                                                                                -- when tags are equal how to combine/fold result of constituents
      , dcfAllTagLtCExpr            :: CExpr                                    -- when tags are < previous
      , dcfAllTagGtCExpr            :: CExpr                                    -- when tags are > previous
      }

type DerivClsMp = Map.Map HsName (AssocL HsName DerivClsFld)
%%]

The table is dependent on builtin names, stored in EHCOpts.

%%[95 export(mkDerivClsMp)
mkDerivClsMp :: EHCOpts -> ValGam -> DataGam -> DerivClsMp
mkDerivClsMp opts valGam dataGam
  = Map.fromList
      [ mk ehbnClassEq [ehbnClassEqFldEq]
           []
           []
           0
           (\_ _ _ _ vs
                -> case vs of
                     [] -> true
                     _  -> foldr1 and vs
           )
           false false
      , mk ehbnClassOrd [ehbnClassOrdFldCompare]
           []
           []
           0
           (\uniq env _ _ vs
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
      , mk ehbnClassShow [ehbnClassShowFldShowsPrec]
           [precDepthNm]
           [CExpr_Int 11] -- dummy, for now, should computed from fixity, etc etc
           1
           (\_ _ tg [dNm] vs
                -> case vs of
                     [] -> showString $ show (ctagNm tg)
                     _  -> mkCExprApp (CExpr_Var $ fn ehbnPrelShowParen)
                             [ true
                             , foldr1 compose
                                 (   [ showString $ show (ctagNm tg) ++ " " ]
                                  ++ intersperse (showString " ") vs
                                 )
                             ]
           )
           undef undef
      ]
  where mk c fs as asSubs omTl cAllMatch cLT cGT
          = (c', map mkf fs)
          where c' = fn c
                mkf f = (f', DerivClsFld f' t as asSubs omTl cAllMatch cLT cGT)
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
        orderingTag conNm
          = dtiCTag
            $ dgiDtiOfCon conNm
            $ panicJust "mkDerivClsMp.dataGamLookup"
            $ dataGamLookup (fn ehbnDataOrdering) dataGam
%%]

